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

module Optical.Ocular where

import           Control.Lens
import           Control.Lens.Internal
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Constraint
import           Data.Constraint.Unsafe
import           Data.Default
import           Data.Functor.Contravariant
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap.Strict         as IM
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Profunctor.Rep
import           Data.Profunctor.Sieve
import qualified Data.Sequence              as S
import           Data.Tagged
import           Data.Tree
import qualified Data.Vector                as B
import           Data.Vector.Generic.Lens

-- | The master class for all things lens. Can be used to defined
--   traversals, lenses, isomorphisms and prisms.
--
--   This class can act as a replacement of Functor, Foldable,
--   Traversable, Pointed, Copointed and Wrapped. All with one function.
--
--   It can also do FunctorWithIndex, FoldableWithIndex and
--   TraversableWithIndex but there are issues with ambiguities if you
--   try to define an indexed LensLike depending only on t.
--
--   The pointed class derived from this class is used to define affine
--   traversals.
class Ocular p q f t where
  optic :: Optical p q f (t a) (t b) a b

-- Mapping -------------------------------------------------------------

type Mappable = Ocular (->) (->) Identity

omap :: Mappable t => (a -> b) -> t a -> t b
omap = over optic

type MappableWithIndex i = Ocular (Indexed i) (->) Identity

iomap :: MappableWithIndex i t => (i -> a -> b) -> t a -> t b
iomap = iover optic

-- Skolem variables (don't export these)
-- The new version of Data.Constraint.Forall uses type families for
-- these but I haven't work out how to use them for this problem yet.
data A
data B

-- Lenses --------------------------------------------------------------

type HasSingle = Ocular (->) (->) (Pretext (->) A B)

single :: HasSingle t => Lens (t a) (t b) a b
single = l where
  l :: forall t a b. HasSingle t => Lens (t a) (t b) a b
  l = cloneLens optic \\ (unsafeCoerceConstraint :: HasSingle t :- Ocular (->) (->) (Pretext (->) a b) t)

type HasSingleWithIndex i = Ocular (Indexed i) (->) (Pretext (Indexed i) A B)

anISingle :: forall i t a b. HasSingleWithIndex i t => AnIndexedLens i (t a) (t b) a b
anISingle = optic \\ (unsafeCoerceConstraint :: HasSingleWithIndex i t :- Ocular (Indexed i) (->) (Pretext (Indexed i) a b) t)

-- even with undefined, this doesn't compile :(
-- isingle :: HasLensWithIndex i t => IndexedLens i (t a) (t b) a b
-- isingle = cloneIndexedLens anISingle

-- Isomorphisms --------------------------------------------------------

type Wrapper = Ocular (Exchange A B) (Exchange A B) Identity

wrap :: Wrapper t => Iso (t a) (t b) a b
wrap = cloneIso anWrap

unwrap :: Wrapper t => Iso a b (t a) (t b)
unwrap = from anWrap

anWrap :: forall t a b. Wrapper t => AnIso (t a) (t b) a b
anWrap = optic \\ (unsafeCoerceConstraint :: Wrapper t :-  Ocular (Exchange a b) (Exchange a b) Identity t)

-- Pointed and copointed -----------------------------------------------

type Copointed = Ocular (->) (->) (Const A)

copoint :: Copointed t => t a -> a
copoint = view copointer

copointer :: forall t a. Copointed t => Getting a (t a) a
copointer = optic \\ (unsafeCoerceConstraint :: Copointed t :- Ocular (->) (->) (Const a) t)

type Pointed = Ocular Tagged Tagged Identity

point :: Pointed t => a -> t a
point = review pointer

pointer :: forall t a. Pointed t => AReview (t a) a
pointer = optic \\ (unsafeCoerceConstraint :: Pointed t :- Ocular Tagged Tagged Identity t)

-- Traversals ----------------------------------------------------------

type HasMultiple = Ocular (->) (->) (Bazaar (->) A B)

multiple :: HasMultiple t => Traversal (t a) (t b) a b
multiple = cloneTraversal l where
  l :: forall t a b. HasMultiple t => ATraversal (t a) (t b) a b
  l = optic \\ (unsafeCoerceConstraint :: HasMultiple t :- Ocular (->) (->) (Bazaar (->) a b) t)

-- Folds ---------------------------------------------------------------

type Folder = Ocular (->) (->) (Const (Endo A))

ofoldr :: forall t a b. Folder t => (a -> b -> b) -> b -> t a -> b
ofoldr = foldrOf optic \\ (unsafeCoerceConstraint :: Folder t :- Ocular (->) (->) (Const (Endo b)) t)

folder :: Folder t => Fold (t a) a
folder = foldring ofoldr

-- Affine traversals ---------------------------------------------------

type AffineTraversal s t a b = forall (f :: * -> *). (Functor f, Pointed f) => (a -> f b) -> s -> f t
type AffineTraversal' s a = AffineTraversal s s a a

type AnAffineTraversal s t a b = (a -> Rude (->) a b b) -> s -> Rude (->) a b t
type AnAffineTraversal' s a = AnAffineTraversal s s a a

newtype Rude p a b t = Rude { runRude :: forall f. (Functor f, Pointed f) => p a (f b) -> f t }

-- Pointed (should be able to generalise)
instance (p ~ Tagged, q ~ p, f ~ Identity) => Ocular p q f (Rude (->) a b) where
  optic = unto (\a -> Rude (const $ point a))

instance Corepresentable p => Sellable p (Rude p) where
  sell = cotabulate $ \ w -> Rude $ tabulate $ \k -> pure (cosieve k w)

cloneAffineT :: AnAffineTraversal s t a b -> AffineTraversal s t a b
cloneAffineT l f = flip runRude f . l sell

type Affine = Ocular (->) (->) (Rude (->) A B)

affine :: Affine t => AffineTraversal (t a) (t b) a b
affine = cloneAffineT anAffine

anAffine :: forall t a b. Affine t => AnAffineTraversal (t a) (t b) a b
anAffine = optic \\ (unsafeCoerceConstraint :: Affine t :- Ocular (->) (->) (Rude (->) a b) t)

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


-- Instances -----------------------------------------------------------

instance (Indexable a p, q ~ (->), Settable f) => Ocular p q f ((->) a) where
  optic = imapped

instance (Profunctor p, p ~ q, Functor f) => Ocular p q f Identity where
  optic = _Wrapped

instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f [] where
  optic = traversed

newtype Static a = Static a
  deriving Show

-- | Pointed (can we combine AReview with Setter (but no getter)?)
instance (p ~ Tagged, q ~ p, f ~ Identity, Default a) => Ocular p q f (Const a) where
  optic = unto (const (Const def))

-- | Getter
instance (p ~ (->), q ~ p, Functor f, Contravariant f) => Ocular p q f Static where
  optic f = phantom . (to (\(Static a) -> a) (phantom . f))

-- | Affine traversal
instance (Choice p, q ~ p, Functor f, Pointed f) => Ocular p q f Maybe where
  optic = prismA Just $ \case Just a -> Right a; Nothing -> Left Nothing

-- | Affine traversal
instance (Choice p, q ~ p, Functor f, Pointed f) => Ocular p q f (Either a) where
  optic = prismA Right $ \case Right a -> Right a; Left a -> Left (Left a)

-- | Affine traversal combined with optic of m
instance (Ocular p q f m, Choice p, p ~ q, Functor f, Pointed f) => Ocular p q f (MaybeT m) where
  optic = _Wrapped . optic . optic

-- instance (Ocular (->) (->) f m, Indexable r p, q ~ (->), Settable f) => Ocular p q f (ReaderT r m) where
--   optic = _Wrapped .> imapped <. optic

instance (Ocular p q f m, q ~ (->), Settable f) => Ocular p q f (ReaderT r m) where
  optic = _Wrapped . mapped . optic

instance (Indexable a p, q ~ (->), Functor f) => Ocular p q f ((,) a) where
  optic f (a,b) = indexed f a b <&> \b' -> (a, b')
  {-# INLINE optic #-}

instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f S.Seq where
  optic = traversed
  {-# INLINE optic #-}

instance (Indexable k p, q ~ (->), Applicative f) => Ocular p q f (M.Map k) where
  optic = itraversed
  {-# INLINE optic #-}

instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f B.Vector where
  optic = vectorTraverse
  {-# INLINE optic #-}

instance (Indexable Int p, q ~ (->), Applicative f) => Ocular p q f IM.IntMap where
  optic = traversed
  {-# INLINE optic #-}

instance (Indexable k p, q ~ (->), Applicative f, Eq k, Hashable k) => Ocular p q f (HM.HashMap k) where
  optic = itraversed
  {-# INLINE optic #-}

instance (Indexable [Int] p, q ~ (->), Applicative f) => Ocular p q f Tree where
  optic = itraversed
  {-# INLINE optic #-}

