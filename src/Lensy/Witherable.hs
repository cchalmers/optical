{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Lensy.Witherable
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Inspired by Fumiaki Kinoshita's "witherable" package but adds extra
-- functions and extends it to indexed witherables.
--
-----------------------------------------------------------------------------
module Lensy.Witherable
  (
    -- * Witherable
    Witherable (..)

  , witherM
  , blightM
  , ordNub
  , hashNub

  , WitherableWithIndex (..)

  -- * Generalization

  -- ** Types
  , WitherLike, WitherLike'
  , Wither, Wither'
  , Withering, Withering'
  , AWither, AWither'
  , Across, Across'

  , IndexedWitherLike , IndexedWitherLike'
  , IndexedWither , IndexedWither'
  , AnIndexedWither , AnIndexedWither'

    -- ** Predefined 'Wither's
  , withered
  , chipped
  , ichipped
  , witherVector
  -- , witherMaybe

    -- ** Using 'Wither's
  , witherOf
  , mapMaybeOf
  , catMaybesOf
  , filterAOf
  , filterOf
  , deleteOf
  , ordNubOf
  , hashNubOf

    -- ** Using indexed 'Wither's
  , iwitherOf
  , imapMaybeOf
  , ifilterAOf
  , ifilterOf

   -- * Cloning

  , Dungeon(..)
  , cloneWither

  -- * Witherable from Traversable
  , Chipped(..)

  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Lens.Internal
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Bool
import qualified Data.Foldable                    as F
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HSet
import qualified Data.IntMap.Lazy                 as IM
import qualified Data.Map.Lazy                    as M
import qualified Data.Maybe                       as Maybe
import           Data.Monoid
import           Data.Orphans                     ()
import           Data.Profunctor.Rep
import           Data.Profunctor.Sieve
import qualified Data.Sequence                    as S
import qualified Data.Set                         as Set
import qualified Data.Traversable                 as T
import qualified Data.Vector                      as V
#if (MIN_VERSION_base(4,7,0))
import           Data.Proxy
#endif
import           Control.Lens
import           Data.Profunctor.Unsafe
import qualified Data.Vector.Generic              as GV
import           Prelude                          hiding (filter)
import qualified Prelude                          as P

-- | 'Traversable' with the ability to remove elements.
--
-- @'traverse' f ≡ 'wither' ('fmap' 'Just' . f)@
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/identity/]
--   @'wither' ('pure' . Just) ≡ 'pure'@
--
-- [/composition/]
--   @Compose . fmap ('wither' f) . 'wither' g ≡ 'wither' (Compose . fmap ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
--   @t . 'wither' f = 'wither' (t . f)@
--
class T.Traversable t => Witherable t where

  -- | Traverse with ability to remove elements by returning 'Nothing'.
  --   This is an applicative version of `mapMaybe`.
  wither :: Wither (t a) (t b) a b -- Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . T.traverse f
  {-# INLINE wither #-}

  -- | Version of map which can throw out elements.
  mapMaybe :: (a -> Maybe b) -> t a -> t b
  mapMaybe = mapMaybeOf wither
  {-# INLINE mapMaybe #-}

  -- | Extract only the 'Just' values, discarding 'Nothing' values.
  catMaybes :: t (Maybe a) -> t a
  catMaybes = catMaybesOf wither
  {-# INLINE catMaybes #-}

  -- | 'filter' with effects.
  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA = filterAOf wither

  -- | Return only the items that satisfy the predicate.
  filter :: (a -> Bool) -> t a -> t a
  filter = filterOf wither
  {-# INLINE filter #-}

  -- | Partition the elements, returning all items that satisfy the
  --   predicate in the 'fst' and other items in the 'snd'.
  --
  --   Equivilent to @('filter' p xs, 'filter' ('not' . p) xs)@.
  partition :: (a -> Bool) -> t a -> (t a, t a)
  partition p t = (filter p t, filter (not . p) t)

  -- | Delete all occurences of an item.
  delete :: (Witherable t, Eq a) => a -> t a -> t a
  delete a = filter (/= a)

  -- | Delete all occurences of a foldable container of items from a
  --   witherable.
  (\\) :: (F.Foldable f, Witherable t, Eq a) => t a -> f a -> t a
  xs \\ t = filter (`F.notElem` v) xs
    where v = V.fromList (F.toList t)

  -- | Delete all occurences not in the foldable container of items.
  intersect :: (F.Foldable f, Witherable t, Eq a) => t a -> f a -> t a
  intersect a b = filter (`V.elem` s) a
    where s = V.fromList (F.toList b)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL wither | mapMaybe | catMaybes #-}
#endif

------------------------------------------------------------------------
-- Wither type
------------------------------------------------------------------------

-- | A 'Wither' with a particular @f@.
type WitherLike f s t a b = (a -> f (Maybe b)) -> s -> f t

-- | A simple 'WitherLike'.
type WitherLike' f s a = WitherLike f s s a a

-- | A description of how to wither an object. 'Wither's can be composed
--   on the left with van Laarhoven lenses (or the more general
--   'Traversal') to form a 'Wither' on the target.
type Wither s t a b = forall f. Applicative f => WitherLike f s t a b

-- | A simple 'Wither'.
type Wither' s a = Wither s s a a

-- | An 'AWither' with a particular profunctor @p@.
type Withering p s t a b = p a (Identity (Maybe b)) -> s -> Identity t

-- | A simple 'Withering'.
type Withering' p s a = Withering p s s a a

-- | A 'Wither' without side effects.
type AWither s t a b = WitherLike Identity s t a b

-- | A simple 'AWither'
type AWither' s a = WitherLike' Identity s a

-- | A 'Wither' for a particular @p@ and @f@.
type Across p f s t a b = Over p f s t a (Maybe b)

-- | A simple 'Across'.
type Across' p f s a = Across p f s s a a

-- Functions on withers ------------------------------------------------

-- | 'witherOf' is actually 'id', but left for consistency.
witherOf :: Applicative f => WitherLike f s t a b -> (a -> f (Maybe b)) -> s -> f t
witherOf = id
{-# INLINE witherOf #-}

-- | Extract all 'Just' values function using the given 'Wither'.
--
-- @
-- 'mapMaybeOf' :: 'Wither' s t a b -> (a -> 'Maybe' b) -> s -> t
-- @
mapMaybeOf :: Profunctor p => Withering p s t a b -> p a (Maybe b) -> s -> t
mapMaybeOf w f = runIdentity #. w (Identity #. f)
{-# INLINE mapMaybeOf #-}

-- | Extract all 'Just' values and discard all 'Nothing' using the given
--   'Wither'.
--
-- @
-- catMaybesOf :: Wither (f (Maybe a)) (f a) (Maybe a) a -> f (Maybe a) -> f a
-- @
catMaybesOf :: AWither s t (Maybe b) b -> s -> t
catMaybesOf w = mapMaybeOf w id
{-# INLINE catMaybesOf #-}

-- | Filter with effects using the given 'Wither'.
--
-- @
-- filterAOf :: Applicative f => 'Wither' s a -> (a -> f 'Bool') -> s -> f s
-- @
filterAOf :: (Conjoined p, Functor f) => Across' p f s a -> p a (f Bool) -> s -> f s
filterAOf w pafb = w $ cotabulate $ \wa -> bool Nothing (Just $ extract wa) <$> cosieve pafb wa
{-# INLINE filterAOf #-}

-- | Delete all occurences of 'a'.
deleteOf :: Eq a => AWither' s a -> a -> s -> s
deleteOf w a = filterOf w (/= a)
{-# INLINE deleteOf #-}

-- | Wither each element of a structure targeted by a 'Wither'.
-- filterOf :: AWither' s a -> (a -> Bool) -> s -> s
filterOf :: Conjoined p => Withering' p s a -> p a Bool -> s -> s
filterOf w f = runIdentity #. filterAOf w (Identity #. f)
{-# INLINE filterOf #-}

-- | A wither for a 'MaybeT'.
witherM :: (Witherable t, Monad m) => (a -> MaybeT m b) -> t a -> m (t b)
witherM f = unwrapMonad . wither (WrapMonad . runMaybeT . f)
{-# INLINE witherM #-}

-- | 'blightM' is 'witherM' with its arguments flipped.
blightM :: (Monad m, Witherable t) => t a -> (a -> MaybeT m b) -> m (t b)
blightM = flip witherM
{-# INLINE blightM #-}

-- | Remove the duplicate elements using a 'Wither'.
ordNubOf :: Ord a => WitherLike' (State (Set.Set a)) s a -> s -> s
ordNubOf w t = evalState (w f t) Set.empty where
  f a = state $ \s ->
    if Set.member a s then (Nothing, s) else (Just a, Set.insert a s)
{-# INLINE ordNubOf #-}

-- | Remove the duplicate elements through a filter. This is often
--   faster than 'ordNubOf', especially when the comparison is expensive.
hashNubOf :: (Eq a, Hashable a) => WitherLike' (State (HSet.HashSet a)) s a -> s -> s
hashNubOf w t = evalState (w f t) HSet.empty where
  f a = state $ \s ->
    if HSet.member a s then (Nothing, s) else (Just a, HSet.insert a s)
{-# INLINE hashNubOf #-}

-- nub :: (Witherable t, Eq a) => t a -> t a
-- nubBy :: Witherable t => (a -> a -> Bool) -> t a -> t a
-- ordNubBy :: Witherable t => (a -> a -> Comparison) -> t a -> t a
-- ordNubOn :: (Witherable t, Ord b) => (a -> b) -> t a -> t a
-- hashNubOf :: (Witherable t, Eq a) => (Hash -> a -> Hash) -> t a -> t a
-- hashOn :: (Witherable t, Hashable a) => t a -> t a

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is much faster than 'nub'.
ordNub :: (Witherable t, Ord a) => t a -> t a
ordNub = ordNubOf wither
{-# INLINE ordNub #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparion (like 'String').
hashNub :: (Witherable t, Eq a, Hashable a) => t a -> t a
hashNub = hashNubOf wither
{-# INLINE hashNub #-}

-- Instances -----------------------------------------------------------

instance Witherable Maybe where
  wither f (Just a) = f a
  wither _ Nothing  = pure Nothing
  {-# INLINE wither #-}

instance Monoid e => Witherable (Either e) where
  wither f (Right a) = maybe (Left mempty) Right <$> f a
  wither _ (Left e)  = pure (Left e)
  {-# INLINE wither #-}

instance Witherable [] where
  wither f = go where
    go (x:xs) = maybe id (:) <$> f x <*> go xs
    go []     = pure []
  {-# INLINE wither #-}
  mapMaybe = Maybe.mapMaybe
  {-# INLINE mapMaybe #-}
  catMaybes = Maybe.catMaybes
  {-# INLINE catMaybes #-}
  filter = P.filter
  {-# INLINE filter #-}

instance Witherable IM.IntMap where
  mapMaybe = IM.mapMaybe
  {-# INLINE mapMaybe #-}
  filter = IM.filter
  {-# INLINE filter #-}

instance Ord k => Witherable (M.Map k) where
  mapMaybe = M.mapMaybe
  {-# INLINE mapMaybe #-}
  filter = M.filter
  {-# INLINE filter #-}

instance (Eq k, Hashable k) => Witherable (HM.HashMap k) where
  wither f = fmap HM.fromList . wither (\(i, a) -> fmap ((,) i) <$> f a) . HM.toList
  {-# INLINABLE wither #-}
  filter = HM.filter
  {-# INLINE filter #-}

#if (MIN_VERSION_base(4,7,0))
instance Witherable Proxy where
  wither _ Proxy = pure Proxy
#endif

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}
  filter = V.filter
  {-# INLINE filter #-}

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}
  filter = S.filter
  {-# INLINE filter #-}

------------------------------------------------------------------------
-- Chipped
------------------------------------------------------------------------

-- | Traversable containers which hold 'Maybe' are witherable.
newtype Chipped t a =
  Chipped { getChipped :: t (Maybe a) } deriving (Functor, F.Foldable, T.Traversable)

deriving instance Show (t (Maybe a)) => Show (Chipped t a)
deriving instance Read (t (Maybe a)) => Read (Chipped t a)
deriving instance Eq (t (Maybe a))   => Eq (Chipped t a)
deriving instance Ord (t (Maybe a))  => Ord (Chipped t a)

instance FunctorWithIndex i t => FunctorWithIndex i (Chipped t) where
  imap f = Chipped #. imap (fmap . f) .# getChipped

instance FoldableWithIndex i t => FoldableWithIndex i (Chipped t) where
  ifoldMap f = maybe mempty id . ifoldMap (fmap . f) .# getChipped

instance TraversableWithIndex i t => TraversableWithIndex i (Chipped t) where
  itraverse f = fmap Chipped . itraverse (traverse . f) .# getChipped

instance Applicative t => Applicative (Chipped t) where
  pure a                  = Chipped $ pure (Just a)
  Chipped f <*> Chipped t = Chipped $ liftA2 (<*>) f t

instance T.Traversable t => Witherable (Chipped t) where
  wither f = fmap Chipped . T.traverse (wither f) . getChipped
  {-# INLINE wither #-}

------------------------------------------------------------------------
-- Dungeon
------------------------------------------------------------------------

newtype Dungeon a b t =
  Dungeon { runDungeon :: forall f. Applicative f => (a -> f (Maybe b)) -> f t }

instance Functor (Dungeon a b) where
  fmap f (Dungeon k) = Dungeon (fmap f . k)
  {-# INLINE fmap #-}

instance Applicative (Dungeon a b) where
  pure a = Dungeon $ const (pure a)
  {-# INLINE pure #-}
  Dungeon f <*> Dungeon g = Dungeon $ \h -> f h <*> g h
  {-# INLINE (<*>) #-}

cloneWither :: WitherLike (Dungeon a b) s t a b -> Wither s t a b
cloneWither l f = (`runDungeon` f) . l (\a -> Dungeon $ \g -> g a)
{-# INLINABLE cloneWither #-}

------------------------------------------------------------------------
-- Indexed Withers
------------------------------------------------------------------------

-- | 'WitherLike' with an index.
type IndexedWitherLike i f s t a b = forall p. Indexable i p => p a (f (Maybe b)) -> s -> f t

-- | A simple 'IndexedWitherLike'.
type IndexedWitherLike' i f s a = IndexedWitherLike i f s s a a

-- | A 'Wither' with an index.
type IndexedWither i s t a b = forall f. Applicative f => IndexedWitherLike i f s t a b

-- | A simple 'IndexedWither'.
type IndexedWither' i s a = IndexedWither i s s a a

-- | 'AWither' with an index.
type AnIndexedWither i s t a b = IndexedWitherLike i Identity s t a b

-- | A simple 'AnIndexedWither'.
type AnIndexedWither' i s a = IndexedWitherLike' i Identity s a

-- | Witherable with access to the index. This class doesn't require any
--   definitions but it is here for more efficient implimentations.
class (TraversableWithIndex i t, Witherable t) => WitherableWithIndex i t | t -> i where
  -- | 'mapMaybe' with an index and effects.
  iwither :: Applicative f => (i -> a -> f (Maybe b)) -> t a -> f (t b)
  iwither f = fmap catMaybes . itraverse f

  -- | An indexed wither than can be combined with indexed lenses.
  iwithered :: IndexedWither i (t a) (t b) a b
  iwithered = conjoined wither (iwither . indexed)

  -- | 'mapMaybe' with an index.
  imapMaybe :: (i -> a -> Maybe b) -> t a -> t b
  imapMaybe = imapMaybeOf iwithered
  {-# INLINE imapMaybe #-}

  -- | Filter with an index and effects.
  ifilterA :: Applicative f => (i -> a -> f Bool) -> t a -> f (t a)
  ifilterA = ifilterAOf iwithered
  {-# INLINE ifilterA #-}

  -- | Filter with an index.
  ifilter :: (i -> a -> Bool) -> t a -> t a
  ifilter = ifilterOf iwithered
  {-# INLINE ifilter #-}

-- Indexed instances ---------------------------------------------------

instance TraversableWithIndex i t => WitherableWithIndex i (Chipped t) where
  iwither f = fmap Chipped . ichipped (Indexed f) .# getChipped
  {-# INLINE iwither #-}

instance WitherableWithIndex Int [] where
  iwither f = go 0 where
    go i (x:xs) = maybe id (:) <$> f i x <*> go (i+1) xs
    go _ []     = pure []
  {-# INLINE iwither #-}
  imapMaybe f = go 0 where
    go i (x:xs) = maybe id (:) (f i x) $ go (i+1) xs
    go _ []     = []
  {-# INLINE imapMaybe #-}
  ifilter f = go 0 where
    go i (x:xs) = bool id (x:) (f i x) $ go (i+1) xs
    go _ []     = []
  {-# INLINE ifilter #-}

instance WitherableWithIndex Int S.Seq where
  iwither f = fmap S.fromList . iwither f . F.toList
  {-# INLINE imapMaybe #-}

instance WitherableWithIndex Int V.Vector where
  iwither f = fmap V.fromList . iwither f . F.toList
  ifilter = V.ifilter
  {-# INLINE ifilter #-}

instance WitherableWithIndex Int IM.IntMap where
  imapMaybe = IM.mapMaybeWithKey
  {-# INLINE imapMaybe #-}
  ifilter = IM.filterWithKey
  {-# INLINE ifilter #-}

instance Ord k => WitherableWithIndex k (M.Map k) where
  imapMaybe = M.mapMaybeWithKey
  {-# INLINE imapMaybe #-}
  ifilter = M.filterWithKey
  {-# INLINE ifilter #-}

instance (Eq k, Hashable k) => WitherableWithIndex k (HM.HashMap k) where
  ifilter = HM.filterWithKey
  {-# INLINE ifilter #-}

instance WitherableWithIndex () Maybe

-- Generalised indexed withers -----------------------------------------

-- | Filter with access to the index.
iwitherOf :: Functor f => IndexedWitherLike i f s t a b -> (i -> a -> f (Maybe b)) -> s -> f t
iwitherOf w = w .# Indexed
{-# INLINE iwitherOf #-}

-- | Filter with access to the index.
ifilterAOf :: Functor f => IndexedWitherLike' i f s a -> (i -> a -> f Bool) -> s -> f s
ifilterAOf w = filterAOf w .# Indexed
{-# INLINE ifilterAOf #-}

-- | Filter with access to the index.
ifilterOf :: AnIndexedWither' i s a -> (i -> a -> Bool) -> s -> s
ifilterOf w = filterOf w .# Indexed
{-# INLINE ifilterOf #-}

imapMaybeOf :: AnIndexedWither i s t a b -> (i -> a -> Maybe b) -> s -> t
imapMaybeOf w = mapMaybeOf w .# Indexed
{-# INLINE imapMaybeOf #-}

------------------------------------------------------------------------
-- Predefined withers
------------------------------------------------------------------------

-- | A wither over a generic vector.
witherVector :: (GV.Vector v a, GV.Vector w b) => IndexedWither Int (v a) (w b) a b
witherVector f = fmap GV.fromList . withered f . GV.toList
{-# INLINE [0] witherVector #-}

{-# RULES
"catMaybe vector wither"
  witherVector = sets (\f -> GV.fromList . mapMaybe f . GV.toList)
    :: (GV.Vector v a, GV.Vector w b) => AWither (v a) (w b) a b
  #-}

-- | 'wither' with an index according to its ordinal position.
withered :: Witherable t => IndexedWither Int (t a) (t b) a b
withered = conjoined wither (indexing wither)
{-# INLINE withered #-}

-- | A wither over a traversable structure of 'Maybe's. Includes an
--   index according to it's ordinal position (including any 'Nothing'
--   values).
chipped :: Traversable t => IndexedWither Int (t (Maybe a)) (t (Maybe b)) a b
chipped = traversed <. wither
{-# INLINE chipped #-}

-- | A wither over a traversable structure of 'Maybe's.
ichipped :: TraversableWithIndex i t => IndexedWither i (t (Maybe a)) (t (Maybe b)) a b
ichipped = itraversed <. wither
{-# INLINE ichipped #-}

