{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Optical.Severable
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Optical.Severable where
  -- (
  --   -- * Witherable
  --   Witherable(..)
  -- , ordNub
  -- , hashNub
  -- -- * Generalization
  -- , WitherLike, Wither, WitherLike', Wither'
  -- , witherOf
  -- , mapMaybeOf
  -- , catMaybesOf
  -- , filterAOf
  -- , filterOf
  -- , ordNubOf
  -- , hashNubOf
  --  -- * Cloning
  -- , cloneWither
  -- , Dungeon(..)
  -- -- * Witherable from Traversable
  -- , Chipped(..)
  -- ) where

import           Control.Comonad
import           Control.Lens.Internal
import           Data.Bifunctor
import           Data.Bool
import           Data.Profunctor.Rep
import qualified Data.IntMap.Lazy                 as IM
import qualified Data.Map.Lazy                    as M
import           Data.Profunctor.Sieve
import qualified Data.Sequence                    as S
import qualified Data.Vector                      as V
import           Control.Applicative
import qualified Data.Foldable                    as F
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Strict
import           Data.Orphans                     ()
import Data.Tuple (swap)
import qualified Data.List as L
import           Data.Profunctor.Unsafe
import           Prelude                          hiding (drop, dropWhile,
                                                   filter, init, span, splitAt,
                                                   take, takeWhile)
import qualified Data.Vector.Generic as GV
import           Control.Lens
import qualified Prelude                          as P

-- | A 'Sever' with a particular @f@.
type SeverLike f s t a b = (a -> f (Maybe b)) -> s -> f (t, s)

-- | A simple 'SeverLike'.
type SeverLike' f s a = SeverLike f s s a a

-- | A description of how to wither an object. 'Sever's can be composed
--   on the left with van Laarhoven lenses (or the more general
--   'Traversal') to form a 'Sever' on the target.
type Sever s t a b = forall f. Applicative f => SeverLike f s t a b

-- | A simple 'Sever'.
type Sever' s a = Sever s s a a

-- | An 'ASever' with a particular profunctor @p@.
type Severing p s t a b = p a (Identity (Maybe b)) -> s -> Identity (t,s)

-- | A simple 'Severing'.
type Severing' p s a = Severing p s s a a

-- | A 'Sever' without side effects.
type ASever s t a b = SeverLike Identity s t a b

-- | A simple 'ASever'
type ASever' s a = SeverLike' Identity s a

-- | A 'Sever' for a particular @p@ and @f@.
type Through p f s t a b = p a (f (Maybe b)) -> s -> f (t, s)

-- | A simple 'Across'.
type Through' p f s a = Through p f s s a a

treverse :: Traversable t => t a -> t a
treverse = partsOf traverse %~ reverse

-- | 'Traversable' with the ability to break into two at some point. The follow
--
-- @'traverse' f = fmap fst . 'sever' (fmap 'Just' . f)@
class Traversable t => Severable t where

  -- | Divide a structure in two by collecting all 'Just' values in the
  --   'fst' tuple. Anything left behind is in the 'snd' tuple.
  sever :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b, t a)

  -- detatch :: (a -> Maybe b) -> t a -> (t b, t a)

  -- | Take up to the first @n@ items.
  take :: Int -> t a -> t a
  take i = fst . splitAt i
  {-# INLINE take #-}

  -- | Drop up to the first @n@ items.
  drop :: Int -> t a -> t a
  drop i = snd . splitAt i
  {-# INLINE drop #-}

  -- split :: Eq a => a -> t a -> (t a, t a)
  -- split = splitOf sever

  -- | Equivilent to @('take' n t, 'drop' n t)@ but can have a more
  --   efficient implimentation.
  splitAt :: Int -> t a -> (t a, t a)
  splitAt = splitAtOf sever
  {-# INLINE splitAt #-}

  -- | Take elements while the predicate is 'True'.
  takeWhileA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  takeWhileA = takeWhileAOf sever

  -- | Take elements while the predicate is 'True'.
  takeWhile :: (a -> Bool) -> t a -> t a
  takeWhile = takeWhileOf sever

  -- | Discard elements while the predicate is 'True'.
  dropWhileA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  dropWhileA = dropWhileAOf sever

  -- | Discard elements while the predicate is 'True'.
  dropWhile :: (a -> Bool) -> t a -> t a
  dropWhile = dropWhileOf sever

  -- | Equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
  span :: (a -> Bool) -> t a -> (t a, t a)
  span = spanOf sever

  -- | Equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
  spanA :: Applicative f => (a -> f Bool) -> t a -> f (t a, t a)
  spanA = spanAOf sever

  -- | 'break' with effects.
  breakA :: Applicative f => (a -> f Bool) -> t a -> f (t a, t a)
  breakA = breakAOf sever

  -- | Equivalent to @'span' ('not' . p)@.
  break :: (a -> Bool) -> t a -> (t a, t a)
  break p = span (not . p)

  -- End variants ------------------------------------------------------

  -- | Take a number of elements from the end of the list.
  --
  -- > takeEnd 3 "hello"  == "llo"
  -- > takeEnd 5 "bye"    == "bye"
  -- > takeEnd (-1) "bye" == ""
  -- > \i xs -> takeEnd i xs `isSuffixOf` xs
  -- > \i xs -> length (takeEnd i xs) == min (max 0 i) (length xs)
  takeEnd :: Int -> t a -> t a
  takeEnd i = treverse . take i . treverse

  -- | Drop a number of elements from the end of the list.
  --
  -- > dropEnd 3 "hello"  == "he"
  -- > dropEnd 5 "bye"    == ""
  -- > dropEnd (-1) "bye" == "bye"
  -- > \i xs -> dropEnd i xs `isPrefixOf` xs
  -- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
  -- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
  dropEnd :: Int -> t a -> t a
  dropEnd i = treverse . drop i . treverse

  -- | @'splitAtEnd' n xs@ returns a split where the second element tries to
  --   contain @n@ elements.
  --
  -- > splitAtEnd 3 "hello" == ("he","llo")
  -- > splitAtEnd 3 "he"    == ("", "he")
  -- > \i xs -> uncurry (++) (splitAt i xs) == xs
  -- > \i xs -> splitAtEnd i xs == (dropEnd i xs, takeEnd i xs)
  splitAtEnd :: Int -> t a -> (t a, t a)
  splitAtEnd i = bimap treverse treverse . swap . splitAt i . treverse

  -- | A version of 'takeWhile' operating from the end.
  --
  -- > takeWhileEnd even [2,3,4,6] == [4,6]
  takeWhileEnd :: (a -> Bool) -> t a -> t a
  takeWhileEnd f = treverse . takeWhile f . treverse

  -- | Discard elements while the predicate is 'True', going from the
  --   end of the structure. This is equivilent to @'Optical.reverse' .
  --   'dropWhile' p . 'Optical.reverse'@.
  dropWhileEnd :: (a -> Bool) -> t a -> t a
  dropWhileEnd f = treverse . dropWhile f . treverse

--   -- | Drops the given prefix if it exists. Returns 'Nothing' if the
--   --   prefix is not present.
--   stripPrefix :: (Foldable f, Witherable t, Eq a) => f a -> t a -> Maybe (t a)
--   stripPrefix p t
--     | b && null l = Just t'
--     | otherwise   = Nothing
--     where
--       (t', (b, l)) = runState (wither f t) (True, F.toList p)
--       f a = state $ \(b, l) -> case l of
--         x:xs -> if x == a
--                 then (Nothing, (b, xs))
--                 else (Just a, (False, []))
--         []   -> (Just a, (b, []))


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL sever #-}
#endif

------------------------------------------------------------------------
-- Wither type
------------------------------------------------------------------------

splitAtOf :: SeverLike' (State Int) s a -> Int -> s -> (s,s)
splitAtOf w n = \t -> evalState (w f t) 0 where
  f a = state $ \i ->
    if i < n then (Nothing, i) else (Just a, i + 1)
{-# INLINE splitAtOf #-}

-- | Take the first @n@ elements from the target of a 'Wither'.
takeOf :: SeverLike' (State Int) s a -> Int -> s -> s
takeOf s n = fst . splitAtOf s n
{-# INLINE takeOf #-}

-- | Drop the first @n@ elements from the target of a 'Wither'.
dropOf :: SeverLike' (State Int) s a -> Int -> s -> s
dropOf s n = snd . splitAtOf s n
{-# INLINE dropOf #-}

splitOf :: Eq a => ASever' s a -> a -> s -> (s,s)
splitOf s a = runIdentity #. s (Identity #. f)
  where f x | x == a    = Nothing
            | otherwise = Just x

-- | Collect all elements while the predicate is 'True' in the left of
--   the tuple, the rest is in the right.
--
-- @
-- spanAOf :: Applicative f => Sever' s a -> (a -> f Bool) -> s -> f (s,s)
-- @
spanAOf :: (Conjoined p, Functor f) => Through' p f s a -> p a (f Bool) -> s -> f (s,s)
spanAOf s pafb = s $ cotabulate $ \wa -> bool Nothing (Just $ extract wa) <$> cosieve pafb wa

-- | Collect all elements while the predicate is 'True' in the left of
--   the tuple, the rest is in the right.
--
-- @
-- spanOf :: Sever' s a -> (a -> Bool) -> s -> (s,s)
-- @
spanOf :: Conjoined p => Severing' p s a -> p a Bool -> s -> (s, s)
spanOf s f = runIdentity #. spanAOf s (Identity #. f)
{-# INLINE spanOf #-}

-- | Equivalent to @'span' ('not' . p)@.
breakAOf :: (Conjoined p, Functor f) => Through' p f s a -> p a (f Bool) -> s -> f (s,s)
breakAOf s p = spanAOf s (fmap not `rmap` p)

-- | Equivalent to @'span' ('not' . p)@.
breakOf :: Conjoined p => Severing' p s a -> p a Bool -> s -> (s, s)
breakOf s p = spanOf s (not `rmap` p)

takeWhileAOf :: (Conjoined p, Functor f) => Through' p f s a -> p a (f Bool) -> s -> f s
takeWhileAOf s p = fmap fst . spanAOf s p

takeWhileOf :: Conjoined p => Severing' p s a -> p a Bool -> s -> s
takeWhileOf s f = fst . spanOf s f
{-# INLINE takeWhileOf #-}

dropWhileAOf :: (Conjoined p, Functor f) => Through' p f s a -> p a (f Bool) -> s -> f s
dropWhileAOf s p = fmap snd . spanAOf s p

dropWhileOf :: Conjoined p => Severing' p s a -> p a Bool -> s -> s
dropWhileOf s f = snd . spanOf s f
{-# INLINE dropWhileOf #-}

-- Instances -----------------------------------------------------------

instance Severable Maybe where
  sever f (Just a) = f a <&> \case b@Just {} -> (b, Nothing); _ -> (Nothing, Just a)
  sever _ Nothing  = pure (Nothing, Nothing)
  {-# INLINE sever #-}

instance Severable [] where
  sever f = go where
    go xss@(x:xs) = maybe (const ([], xss)) (first . (:)) <$> f x <*> go xs
    go []         = pure ([], [])
  {-# INLINE sever #-}
  take = P.take
  drop = P.drop
  -- split a = span (==a)
  splitAt = P.splitAt
  takeWhile = P.takeWhile
  dropWhile = P.dropWhile
  span = P.span
  break = P.break
  takeWhileEnd f = P.reverse . P.takeWhile f . P.reverse
  dropWhileEnd = L.dropWhileEnd
  takeEnd i xs0 = f xs0 (drop i xs0)
    where f (_:xs) (_:ys) = f xs ys
          f xs     _      = xs
  dropEnd i xs0 = f xs0 (drop i xs0)
    where f (x:xs) (_:ys) = x : f xs ys
          f _      _      = []
  {-# INLINE take         #-}
  {-# INLINE drop         #-}
  {-# INLINE splitAt      #-}
  {-# INLINE takeWhile    #-}
  {-# INLINE dropWhile    #-}
  {-# INLINE span         #-}
  {-# INLINE break        #-}
  {-# INLINE takeWhileEnd        #-}
  {-# INLINE dropWhileEnd        #-}

instance Severable IM.IntMap where
  sever f = fmap (bimap IM.fromList IM.fromAscList) . sever (\(i, a) -> fmap ((,) i) <$> f a) . IM.toList
  {-# INLINE sever #-}
  take n = IM.fromAscList . P.take n . IM.toAscList
  {-# INLINE take #-}
  drop n = IM.fromAscList . P.drop n . IM.toAscList
  {-# INLINE drop #-}

instance Ord k => Severable (M.Map k) where
  sever f = fmap (bimap M.fromList M.fromAscList) . sever (\(i, a) -> fmap ((,) i) <$> f a) . M.toList
  {-# INLINE sever #-}
  take n = M.fromAscList . P.take n . M.toAscList
  {-# INLINE take #-}
  drop n = M.fromAscList . P.drop n . M.toAscList
  {-# INLINE drop #-}

instance Severable V.Vector where
  sever f = fmap (bimap V.fromList V.fromList) . sever f . V.toList
  {-# INLINE sever #-}
  take = V.take
  {-# INLINE take #-}
  drop = V.drop
  {-# INLINE drop #-}
  splitAt = V.splitAt
  {-# INLINE splitAt #-}
  takeWhile = V.takeWhile
  {-# INLINE takeWhile #-}
  dropWhile = V.dropWhile
  {-# INLINE dropWhile #-}

instance Severable S.Seq where
  sever f = fmap (bimap S.fromList S.fromList) . sever f . F.toList
  {-# INLINE sever #-}
  take = S.take
  {-# INLINE take #-}
  drop = S.drop
  {-# INLINE drop #-}
  splitAt = S.splitAt
  {-# INLINE splitAt #-}
  takeWhile = S.takeWhileL
  {-# INLINE takeWhile #-}
  dropWhile = S.dropWhileL
  {-# INLINE dropWhile #-}

------------------------------------------------------------------------
-- Indexed Severs
------------------------------------------------------------------------

type IndexedSeverLike i f s t a b = forall p. Indexable i p => p a (f (Maybe b)) -> s -> f (t,s)
type IndexedSeverLike' i f s a    = IndexedSeverLike i f s s a a
type IndexedSever i s t a b       = forall f. Applicative f => IndexedSeverLike i f s t a b
type IndexedSever' i s a          = IndexedSever i s s a a
type AnIndexedSever i s t a b     = IndexedSeverLike i Identity s t a b
type AnIndexedSever' i s a        = IndexedSeverLike' i Identity s a

class (TraversableWithIndex i t, Severable t) => SeverableWithIndex i t | t -> i where
  isever :: Applicative f => (i -> a -> f (Maybe b)) -> t a -> f (t b, t a)

  isevered :: IndexedSever i (t a) (t b) a b
  isevered = conjoined sever (isever . indexed)

    -- | Take elements while the predicate is 'True'.
  itakeWhile :: (i -> a -> Bool) -> t a -> t a
  itakeWhile = itakeWhileOf isevered

    -- | Take elements while the predicate is 'True'.
  itakeWhileA :: Applicative f => (i -> a -> f Bool) -> t a -> f (t a)
  itakeWhileA = itakeWhileAOf isevered

  -- | Discard elements while the predicate is 'True'.
  idropWhileA :: Applicative f => (i -> a -> f Bool) -> t a -> f (t a)
  idropWhileA = idropWhileAOf isevered

  -- | Discard elements while the predicate is 'True'.
  idropWhile :: (i -> a -> Bool) -> t a -> t a
  idropWhile = idropWhileOf isevered

  -- -- | Discard elements while the predicate is 'True', going from the
  -- --   end of the structure. This is equivilent to @'Optical.reverse' .
  -- --   'dropWhile' p . 'Optical.reverse'@.
  -- dropWhileEnd :: (a -> Bool) -> t a -> t a

  -- | Equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
  ispan :: (i -> a -> Bool) -> t a -> (t a, t a)
  ispan = ispanOf isevered

  -- | Equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@.
  ispanA :: Applicative f => (i -> a -> f Bool) -> t a -> f (t a, t a)
  ispanA = ispanAOf isevered

  -- | 'break' with effects.
  ibreakA :: Applicative f => (i -> a -> f Bool) -> t a -> f (t a, t a)
  ibreakA = ibreakAOf isevered

  -- | Equivalent to @'span' ('not' . p)@.
  ibreak :: (i -> a -> Bool) -> t a -> (t a, t a)
  ibreak = ibreakOf isevered

itakeWhileAOf :: Functor f => IndexedSeverLike' i f s a -> (i -> a -> f Bool) -> s -> f s
itakeWhileAOf s = takeWhileAOf s .# Indexed
{-# INLINE itakeWhileAOf #-}

idropWhileAOf :: Functor f => IndexedSeverLike' i f s a -> (i -> a -> f Bool) -> s -> f s
idropWhileAOf s = dropWhileAOf s .# Indexed
{-# INLINE idropWhileAOf #-}

itakeWhileOf :: AnIndexedSever' i s a -> (i -> a -> Bool) -> s -> s
itakeWhileOf s = takeWhileOf s .# Indexed
{-# INLINE itakeWhileOf #-}

idropWhileOf :: AnIndexedSever' i s a -> (i -> a -> Bool) -> s -> s
idropWhileOf s = dropWhileOf s .# Indexed
{-# INLINE idropWhileOf #-}

ibreakAOf :: Functor f => IndexedSeverLike' i f s a -> (i -> a -> f Bool) -> s -> f (s, s)
ibreakAOf s = breakAOf s .# Indexed
{-# INLINE ibreakAOf #-}

ibreakOf :: AnIndexedSever' i s a -> (i -> a -> Bool) -> s -> (s, s)
ibreakOf s = breakOf s .# Indexed
{-# INLINE ibreakOf #-}

ispanAOf :: Functor f => IndexedSeverLike' i f s a -> (i -> a -> f Bool) -> s -> f (s, s)
ispanAOf s = spanAOf s .# Indexed
{-# INLINE ispanAOf #-}

ispanOf :: AnIndexedSever' i s a -> (i -> a -> Bool) -> s -> (s, s)
ispanOf s = spanOf s .# Indexed
{-# INLINE ispanOf #-}

instance SeverableWithIndex Int [] where
  isever f = go 0 where
    go i xss@(x:xs) = maybe (const ([], xss)) (first . (:)) <$> f i x <*> go (i+1) xs
    go _ []         = pure ([], [])
  {-# INLINE isever #-}

instance SeverableWithIndex Int S.Seq where
  isever f = fmap (bimap S.fromList S.fromList) . isever f . F.toList

instance SeverableWithIndex Int V.Vector where
  isever f = fmap (bimap V.fromList V.fromList) . isever f . V.toList

-- ------------------------------------------------------------------------
-- -- Predefined severs
-- ------------------------------------------------------------------------

-- | A sever over a generic vector.
severVector :: (GV.Vector v a, GV.Vector w b) => IndexedSever Int (v a) (w b) a b
severVector f = fmap (bimap GV.fromList GV.fromList) . severed f . GV.toList
{-# INLINE severVector #-}

-- {-# RULES
--   "catMaybe vector sever"
--     severVector = sets (\f -> GV.fromList . mapMaybe f . GV.toList)
--       :: (GV.Vector v a, GV.Vector w b) => ASever (v a) (w b) a b
--   #-}

-- | 'sever' with an index according to its ordinal position.
severed :: Severable t => IndexedSever Int (t a) (t b) a b
severed = conjoined sever (indexing sever)
{-# INLINE severed #-}

