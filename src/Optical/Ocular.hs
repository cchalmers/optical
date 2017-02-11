{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Optical.Ocular
  ( Ocular (..)
  , IOcular

  -- * Wrappers
  , Wrapper
  , wrap
  , unwrap

  -- ** Pointed
  , Pointed
  , point
  , Copointed
  , copoint

  -- * Lenses
  , Single
  , single
  , isingle

  -- * Affine traversals
  , AffineTraversal
  , AffineTraversal'
  , AnAffineTraversal
  , AnAffineTraversal'
  , Rude
  , cloneAffineT

  -- ** Affine
  , Affine
  , affine

  -- * Affine prisms
  , PrismA
  , PrismA'
  , prismA
  , prismA'
  , clonePrismA

  -- ** Prismic
  , Prismic
  , prismic

  -- * Clones of classes in base and lens
  -- ** Functor(WithIndex) clone
  , Mappable
  , MappableWithIndex
  , omap
  , oimap

  -- ** Foldable(WithIndex) clone
  , Folder
  , FolderWithIndex
  , folder
  , ifolder

  -- ** Traversable(WithIndex) clone
  , Multiple
  , MultipleWithIndex
  , multiple
  , imultiple
  ) where

import           Control.Lens hiding (Traversing)
import           Control.Lens.Internal
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Constraint
import           Data.Constraint.Unsafe
import           Data.Default
import           Data.Hashable
import           Data.HashMap.Lazy        (HashMap)
import           Data.IntMap         (IntMap)
import           Data.Map            (Map)
import           Data.Monoid
import           Data.Profunctor.Rep
import           Data.Profunctor.Sieve
-- import           Data.Profunctor.Unsafe
import           Data.Proxy
import           Data.Sequence              (Seq)
import           Data.Tagged
import           Data.Tree
-- import Data.Coerce
-- import  Data.Profunctor.Traversing
-- import Data.Profunctor.Strong
-- import Data.Profunctor.Closed
import           Data.Vector                (Vector)
import           Data.Vector.Generic.Lens
-- import Data.Functor.Contravariant

-- | The master class for all things lens.
--
--   This class can also act as a replacement of Functor, Foldable,
--   Traversable, Pointed, Copointed, FunctorWithIndex,
--   FoldableWithIndex and TraversableWithIndex. The class can also
--   define lenses, indexed lenses, isomorphisms, affine traversals and
--   prisms.
--
--   The pointed class derived from this class is used to define affine
--   traversals.
class Ocular p q f t where
  -- | Type used for indexed optics
  type I (t :: * -> *)
  type I t = ()

  -- | The most general optic available for @t@.
  optic :: Optical p q f (t a) (t b) a b

-- | An 'Ocular' with an index i.
class (I t ~ i, Ocular (Indexed i) (->) f t) => IOcular i f t
instance (I t ~ i, Ocular (Indexed i) (->) f t) => IOcular i f t

-- Skolem variables (don't export these)
-- The new version of Data.Constraint.Forall uses type families for
-- these but I haven't work out how to use them for this problem yet.
data A
data B

-- Lenses --------------------------------------------------------------

-- | Types with exactly one value corresponding the to last parameter.
--
-- @
-- 'Single' 'Identity'
-- 'Single' ((,) a)
-- @
type Single = Ocular (->) (->) (Pretext (->) A B)

-- | Lens onto the single value in the last parameter.
single :: Single t => Lens (t a) (t b) a b
single = l where
  l :: forall t a b. Single t => Lens (t a) (t b) a b
  l = cloneLens optic \\ (unsafeCoerceConstraint :: Single t :- Ocular (->) (->) (Pretext (->) a b) t)
{-# INLINE single #-}

-- | Types with exactly one value corresponding the to last parameter
--   and an index @i@
--
-- @
-- 'SingleWithIndex' a ((,) a)
-- @
type SingleWithIndex i = IOcular i (Pretext (Indexed i) A B)

isingle :: SingleWithIndex i t => IndexedLens i (t a) (t b) a b
isingle = cloneIndexedLens anISingle
{-# INLINE isingle #-}

anISingle :: forall i t a b. SingleWithIndex i t => AnIndexedLens i (t a) (t b) a b
anISingle = optic \\ (unsafeCoerceConstraint :: SingleWithIndex i t :- Ocular (Indexed i) (->) (Pretext (Indexed i) a b) t)
{-# INLINE anISingle #-}

-- Isomorphisms --------------------------------------------------------

-- | Types that are only wrappers.
--
-- @
-- 'Wrapper' 'Identity'
-- 'Wrapper' ('Tagged' k)
-- @
type Wrapper = Ocular (Exchange A B) (Exchange A B) Identity

wrap :: Wrapper t => Iso (t a) (t b) a b
wrap = cloneIso anWrap
{-# INLINE wrap #-}

unwrap :: Wrapper t => Iso a b (t a) (t b)
unwrap = from anWrap
{-# INLINE unwrap #-}

anWrap :: forall t a b. Wrapper t => AnIso (t a) (t b) a b
anWrap = optic \\ (unsafeCoerceConstraint :: Wrapper t :-  Ocular (Exchange a b) (Exchange a b) Identity t)
{-# INLINE anWrap #-}

-- Pointed and copointed -----------------------------------------------

-- | Types with just one value that can be extracted.
--
-- @
-- 'Copointed' 'Identity'
-- 'Copointed' 'Tagged'
-- 'Copointed' ((,) a)
-- @
type Copointed = Ocular (->) (->) (Const A)

copoint :: Copointed t => t a -> a
copoint = view copointer
{-# INLINE copoint #-}

copointer :: forall t a. Copointed t => Getting a (t a) a
copointer = optic \\ (unsafeCoerceConstraint :: Copointed t :- Ocular (->) (->) (Const a) t)
{-# INLINE copointer #-}

-- | Types that can be contructed from a single value.
--
-- @
-- 'Pointed' 'Identity'
-- 'Pointed' 'Proxy'
-- 'Pointed' 'Tagged'
-- 'Pointed' '[]'
-- 'Pointed' 'Maybe'
-- 'Pointed' ((,) a)
-- @
type Pointed = Ocular Tagged Tagged Identity

-- | Contruct from a single value.
point :: Pointed t => a -> t a
point = review pointer
{-# INLINE point #-}

pointer :: forall t a. Pointed t => AReview (t a) a
pointer = optic \\ (unsafeCoerceConstraint :: Pointed t :- Ocular Tagged Tagged Identity t)
{-# INLINE pointer #-}


-- Affine traversals ---------------------------------------------------

type AffineTraversal s t a b = forall (f :: * -> *). (Functor f, Pointed f) => (a -> f b) -> s -> f t
type AffineTraversal' s a = AffineTraversal s s a a

type AnAffineTraversal s t a b = (a -> Rude (->) a b b) -> s -> Rude (->) a b t
type AnAffineTraversal' s a = AnAffineTraversal s s a a

newtype Rude p a b t = Rude { runRude :: forall f. (Functor f, Pointed f) => p a (f b) -> f t }

instance Functor (Rude p a b) where
  fmap f (Rude k) = Rude (fmap f . k)
  {-# INLINE fmap #-}

-- Pointed (should be able to generalise)
instance (p ~ Tagged, q ~ p, f ~ Identity) => Ocular p q f (Rude (->) a b) where
  optic = unto (\a -> Rude (const $ point a))
  {-# INLINE optic #-}

instance Corepresentable p => Sellable p (Rude p) where
  sell = cotabulate $ \ w -> Rude $ tabulate $ \k -> pure (cosieve k w)
  {-# INLINE sell #-}

cloneAffineT :: AnAffineTraversal s t a b -> AffineTraversal s t a b
cloneAffineT l f = flip runRude f . l sell
{-# INLINE cloneAffineT #-}

type Affine = Ocular (->) (->) (Rude (->) A B)

affine :: Affine t => AffineTraversal (t a) (t b) a b
affine = cloneAffineT anAffine
{-# INLINE affine #-}

anAffine :: forall t a b. Affine t => AnAffineTraversal (t a) (t b) a b
anAffine = optic \\ (unsafeCoerceConstraint :: Affine t :- Ocular (->) (->) (Rude (->) a b) t)
{-# INLINE anAffine #-}

-- Prisms --------------------------------------------------------------

type PrismA s t a b = forall p f. (Choice p, Functor f, Pointed f) => p a (f b) -> p s (f t)
type PrismA' s a = PrismA s s a a

-- | Build a 'Control.Lens.Prism.Prism'.
--
-- @'Either' t a@ is used instead of @'Maybe' a@ to permit the types of @s@ and @t@ to differ.
--
prismA :: (b -> t) -> (s -> Either t a) -> PrismA s t a b
prismA bt seta = dimap seta (either point (fmap bt)) . right'
{-# INLINE prismA #-}

-- | This is usually used to build a 'Prism'', when you have to use an operation like
-- 'Data.Typeable.cast' which already returns a 'Maybe'.
prismA' :: (b -> s) -> (s -> Maybe a) -> PrismA s s a b
prismA' bs sma = prismA bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prismA' #-}

clonePrismA :: APrism s t a b -> PrismA s t a b
clonePrismA k = withPrism k prismA
{-# INLINE clonePrismA #-}

-- | Types with a prism onto the last value.
--
-- @
-- 'Prismic' 'Maybe'
-- @
type Prismic = Ocular (Market A B) (Market A B) Identity

prismic :: Prismic t => PrismA (t a) (t b) a b
prismic = clonePrismA anPrismic

anPrismic :: forall t a b. Prismic t => APrism (t a) (t b) a b
anPrismic = optic \\ (unsafeCoerceConstraint :: Prismic t :- Ocular (Market a b) (Market a b) Identity t)

-- Traversals and folds ------------------------------------------------

-- These are just copies of Functor(WithIndex), Traversable(WithIndex)
-- and Foldable(WithIndex). I've left them here for completeness. (Note
-- performance of the traversals is probably not optimal (performance of
-- setters and folds should be as performant as the original optic.))

-- Functor -------------------------------------------------------------

-- | Same as 'Functor'.
type Mappable = Ocular (->) (->) Identity

-- | Same as 'fmap'.
omap :: Mappable t => (a -> b) -> t a -> t b
omap = over optic
{-# INLINE omap #-}

-- | Same as @'FunctorWithIndex' i@
type MappableWithIndex i = IOcular i Identity

-- | Same as 'imap'.
oimap :: MappableWithIndex i t => (i -> a -> b) -> t a -> t b
oimap = iover optic
{-# INLINE oimap #-}

-- Traversal -----------------------------------------------------------

-- | Same as 'Traversable'.
type Multiple = Ocular (->) (->) (Bazaar (->) A B)

-- | Same as 'traverse'.
multiple :: Multiple t => Traversal (t a) (t b) a b
multiple = cloneTraversal l where
  l :: forall t a b. Multiple t => ATraversal (t a) (t b) a b
  l = optic \\ (unsafeCoerceConstraint :: Multiple t :- Ocular (->) (->) (Bazaar (->) a b) t)
{-# INLINE multiple #-}

-- | Same as @'TraversableWithIndex' i@.
type MultipleWithIndex i = IOcular i (Bazaar (Indexed i) A B)

-- | Same as 'itraversed'.
imultiple :: MultipleWithIndex i t => IndexedTraversal i (t a) (t b) a b
imultiple = cloneIndexedTraversal l where
  l :: forall i t a b. MultipleWithIndex i t => AnIndexedTraversal i (t a) (t b) a b
  l = optic \\ (unsafeCoerceConstraint :: MultipleWithIndex i t :- Ocular (Indexed i) (->) (Bazaar (Indexed i) a b) t)
{-# INLINE imultiple #-}

-- Folds ---------------------------------------------------------------

-- | Same as 'Foldable'.
type Folder = Ocular (->) (->) (Const (Endo A))

-- | Same as 'foldr'.
ofoldr :: forall t a b. Folder t => (a -> b -> b) -> b -> t a -> b
ofoldr = foldrOf optic \\ (unsafeCoerceConstraint :: Folder t :- Ocular (->) (->) (Const (Endo b)) t)
{-# INLINE ofoldr #-}

-- | Same as 'folded'.
folder :: Folder t => Fold (t a) a
folder = foldring ofoldr
{-# INLINE folder #-}

-- | Same as 'Foldable'.
type FolderWithIndex i = IOcular i (Const (Endo B))

-- | Same as 'foldr'.
oifoldr :: forall i t a b. FolderWithIndex i t => (i -> a -> b -> b) -> b -> t a -> b
oifoldr = ifoldrOf optic \\ (unsafeCoerceConstraint :: Ocular (Indexed i) (->) (Const (Endo B)) t :- Ocular (Indexed i) (->) (Const (Endo b)) t)
-- XXX Why does using FolderWithIndex here cause a ghc panic?
-- oifoldr = ifoldrOf optic \\ (unsafeCoerceConstraint :: FolderWithIndex i t :- Ocular (Indexed i) (->) (Const (Endo b)) t)
{-# INLINE oifoldr #-}

-- | Same as 'folded'.
ifolder :: FolderWithIndex i t => IndexedFold i (t a) a
ifolder = ifoldring oifoldr
{-# INLINE ifolder #-}


-- Instances -----------------------------------------------------------

-- | @'IndexedSetter' a (a -> b) (a -> b') b b'@
instance (Indexable a p, q ~ (->), Settable f) => Ocular p q f ((->) a) where
  type I ((->) a) = a
  optic = imapped
  {-# INLINE optic #-}

-- | @'Iso' ('Identity' a) ('Identity' b) a b@
instance (Profunctor p, p ~ q, Functor f) => Ocular p q f Identity where
  optic = _Wrapped
  {-# INLINE optic #-}

-- | @'IndexedTraversal' i [a] [b] a b@
instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f [] where
  type I [] = Int
  optic = traversed
  {-# INLINE optic #-}

-- | Pointed (can we combine AReview with Setter (but no getter)?)
instance (Bifunctor p, Profunctor p, q ~ p, Settable f) => Ocular p q f Proxy where
  optic = unto (const Proxy)
  -- optic = unto' (const Proxy) phantom
  {-# INLINE optic #-}

-- | Pointed (can we combine AReview with Setter (but no getter)?)
instance (p ~ Tagged, q ~ p, f ~ Identity, Default a) => Ocular p q f (Const a) where
  optic = unto (const (Const def))
  {-# INLINE optic #-}

-- | 'PrismA' ('Maybe' a) ('Maybe' b) a b
instance (Choice p, q ~ p, Functor f, Pointed f) => Ocular p q f Maybe where
  optic = prismA Just $ \case Just a -> Right a; Nothing -> Left Nothing
  {-# INLINE optic #-}

-- | 'PrismA' ('Either' a b) ('Either' a b') b b'
instance (Choice p, q ~ p, Functor f, Pointed f) => Ocular p q f (Either a) where
  optic = prismA Right $ \case Right a -> Right a; Left a -> Left (Left a)
  {-# INLINE optic #-}

-- | @'Prismic' m => 'PrismA' ('MaybeT' m a) ('MaybeT' m b) a b
instance (Ocular p q f m, Choice p, p ~ q, Functor f, Pointed f) => Ocular p q f (MaybeT m) where
  optic = _Wrapped . optic . optic
  {-# INLINE optic #-}

-- instance (Ocular (->) (->) f m, Indexable r p, q ~ (->), Settable f) => Ocular p q f (ReaderT r m) where
--   optic = _Wrapped .> imapped <. optic

-- | @'Functor' m => 'Setter' ('ReaderT' r m a) ('ReaderT r m b) a b'
instance (Ocular p q f m, q ~ (->), Settable f) => Ocular p q f (ReaderT r m) where
  optic = _Wrapped . mapped . optic
  {-# INLINE optic #-}

-- | @'IndexedLens' a (a,b) (a,b') b b'
instance (Indexable a p, q ~ (->), Functor f) => Ocular p q f ((,) a) where
  type I ((,) a) = a
  optic f (a,b) = indexed f a b <&> \b' -> (a, b')
  {-# INLINE optic #-}

-- | @'IndexedTraversal' Int ('Seq' a) ('Seq' b) a b@
instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f Seq where
  type I Seq = Int
  optic = traversed
  {-# INLINE optic #-}

-- | @'IndexedTraversal' k ('Map' k a) ('Map' k b) a b@
instance (Indexable k p, q ~ (->), Applicative f) => Ocular p q f (Map k) where
  type I (Map k) = k
  optic = itraversed
  {-# INLINE optic #-}

-- | @'IndexedTraversal' i ('Vector' a) ('Vector' b) a b@
instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f Vector where
  type I Vector = Int
  optic = vectorTraverse
  {-# INLINE optic #-}

-- | @'IndexedTraversal' i ('Vector' a) ('Vector' b) a b@
instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f IntMap where
  type I IntMap = Int
  optic = traversed
  {-# INLINE optic #-}

-- | @'IndexedTraversal' k ('HashMap' k a) ('HashMap' k b) a b@
instance (Indexable k p, q ~ (->), Applicative f, Eq k, Hashable k) => Ocular p q f (HashMap k) where
  type I (HashMap k) = k
  optic = itraversed
  {-# INLINE optic #-}

-- | @'IndexedTraversal' [Int] ('Tree' k a) ('Tree' k b) a b@
instance (Indexable [Int] p, q ~ (->), Applicative f) => Ocular p q f Tree where
  type I Tree = [Int]
  optic = itraversed
  {-# INLINE optic #-}

