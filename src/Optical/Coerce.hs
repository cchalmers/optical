{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RoleAnnotations #-}
module Optical.Coerce where
  -- ( Coercible1 (..)
  -- , coerce1
  -- , liftC

  -- , Coercible2 (..)
  -- , coerce2
  -- , lift2C

  -- , CoercedIso
  -- , CoercedIso'
  -- , coerced
  -- ) where

import Control.Lens hiding (coerced)
import Data.Type.Coercion
import Data.Coerce
import Data.Proxy
import Data.Vector (Vector)
import Data.Sequence (Seq)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Tagged
import Control.Monad.RWS
import Data.Ord
import Control.Monad.ST (ST)
import Control.Monad.Trans.Identity
import Data.Reflection
import Data.Foldable
-- import Data.Proxy

import Prelude hiding ((.), id)
import Control.Category
import Data.Constraint.Lifting
import Data.Constraint
import Data.Functor.Classes
import Linear (V0, V1, V2, V3, V4)
import Unsafe.Coerce
import Data.Vector.Unboxed (Unbox)

-- | Objects with the ability to coerce given the argument is
--   'Coercible'. This is usually defined as @'coercion1' = 'Coersion'@
--   as long as the constructor is in scope or the type role is not
--   nominal.
class Coercible1 f where
  coercion1 :: Coercible a b => Coercion (f a) (f b)

-- | Coerce over a 'Coercible1'.
coerce1 :: (Coercible1 f, Coercible a b) => f a -> f b
coerce1 = coerceWith coercion1

coercionOf :: Coercible a b => (a -> b) -> Coercion a b
coercionOf _ = Coercion

-- | Lift a 'Coercion' into the 'Coercible1'.
liftC :: Coercible1 f => Coercion a b -> Coercion (f a) (f b)
liftC Coercion = coercion1

instance Coercible1 ((,) a)    where coercion1 = Coercion
instance Coercible1 ((->) a)   where coercion1 = Coercion
instance Coercible1 (Const m)  where coercion1 = Coercion
instance Coercible1 (Either a) where coercion1 = Coercion
instance Coercible1 (Map k)    where coercion1 = Coercion
instance Coercible1 (ST s)     where coercion1 = Coercion
instance Coercible1 Down       where coercion1 = Coercion
instance Coercible1 First      where coercion1 = Coercion
instance Coercible1 IO         where coercion1 = Coercion
instance Coercible1 Identity   where coercion1 = Coercion
instance Coercible1 IntMap     where coercion1 = Coercion
instance Coercible1 Last       where coercion1 = Coercion
instance Coercible1 Maybe      where coercion1 = Coercion
instance Coercible1 Product    where coercion1 = Coercion
instance Coercible1 Proxy      where coercion1 = Coercion
instance Coercible1 Seq        where coercion1 = Coercion
instance Coercible1 Sum        where coercion1 = Coercion
instance Coercible1 Vector     where coercion1 = Coercion
instance Coercible1 []         where coercion1 = Coercion
instance Coercible1 V0         where coercion1 = Coercion
instance Coercible1 V1         where coercion1 = Coercion
instance Coercible1 V2         where coercion1 = Coercion
instance Coercible1 V3         where coercion1 = Coercion
instance Coercible1 V4         where coercion1 = Coercion

-- Just having Coercible (m (a, s)) (m (b, s)) in scope isn't enough.
-- You have to really guide it.
instance Coercible1 m => Coercible1 (StateT s m) where
  coercion1 = coercionOf StateT . liftC coercion1 . coercionOf runStateT

instance Coercible1 m => Coercible1 (ReaderT r m) where
  coercion1 = coercionOf ReaderT . liftC coercion1 . coercionOf runReaderT

instance Coercible1 m => Coercible1 (WriterT r m) where
  coercion1 = coercionOf WriterT . coercion1 . coercionOf runWriterT

instance Coercible1 m => Coercible1 (RWST r w s m) where
  coercion1 = coercionOf RWST . liftC (liftC coercion1) . coercionOf runRWST

instance Coercible1 f => Coercible1 (Alt f) where
  coercion1 = coercionOf Alt . coercion1 . coercionOf getAlt

instance Coercible1 f => Coercible1 (IdentityT f) where
  coercion1 = coercionOf IdentityT . coercion1 . coercionOf runIdentityT

-- | Like 'ala' but using coercions.
-- alac :: (Coercible1 f, Coercible a s) => (a -> s) -> ((a -> s) -> f s) -> f a
-- alac k f = coerce1 (f k)

-- Poor half-solution for allowing generalised newtype deriving with
-- join in Monad

class Monad m => Join m where
  mjoin :: m (m a) -> m a
  mjoin = join

-- joinC :: forall f g a. (Coercible1 f, Coercible f g) => f (f a) -> g (g a)
-- joinC = coerce (coerce1 :: f (f a) -> f (g a))

joinCoercion :: forall f g a. (Coercible1 f, Coercible f g) => Coercion (f (f a) -> f a) (g (g a) -> g a)
joinCoercion = contratrans (Coercion . (coercion1 :: Coercion (f (f a)) (f (g a)))) . coercion1

joinWith :: forall proxy f g a. (Coercible1 f, Join f, Coercible f g) => proxy f -> g (g a) -> g a
joinWith _ = coerceWith joinCoercion (join :: f (f a) -> f a)

contratrans :: Coercion a b -> Coercion (a -> c) (b -> c)
contratrans Coercion = Coercion

instance (Coercible1 m, Join m) => Join (IdentityT m) where
  mjoin = joinWith (Proxy :: Proxy m)

class Coercible2 p where
  coercion2 :: (Coercible a b, Coercible s t) => Coercion (p a s) (p b t)

instance Coercible2 (->)        where coercion2 = Coercion
instance Coercible2 (Indexed i) where coercion2 = Coercion
instance Coercible2 Tagged      where coercion2 = Coercion

lift2C :: Coercible2 p => Coercion a b -> Coercion s t -> Coercion (p a s) (p b t)
lift2C Coercion Coercion = coercion2

coerce2 :: (Coercible2 p, Coercible a b, Coercible s t) => p a s -> p b t
coerce2 = coerceWith coercion2

type CoercedIso s t a b = forall (p :: * -> * -> *) (f :: * -> *). (Coercible2 p, Coercible1 f) => p a (f b) -> p s (f t)

type CoercedIso' s a = CoercedIso s s a a

-- | An 'Iso' between two coercibles.
coerced :: (Coercible a s, Coercible b t) => CoercedIso s t a b
coerced = coerceWith (lift2C Coercion coercion1)

------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------

-- Eq ------------------------------------------------------------------

newtype ReifiedEq a = ReifiedEq { runEq :: a -> a -> Bool }

newtype ReflectedEq a s = ReflectedEq a

instance Reifies s (ReifiedEq a) => Eq (ReflectedEq a s) where
  (==) = coerce (reflect (Proxy :: Proxy s))

liftingEq :: forall f a. (Coercible1 f, Lifting Eq f) => (a -> a -> Bool) -> f a -> f a -> Bool
liftingEq f a b = reify (ReifiedEq f) (\p -> umm p (==) a b \\ subsEq p)
  where
    subsEq :: Proxy s -> Eq (ReflectedEq a s) :- Eq (f (ReflectedEq a s))
    subsEq _ = lifting

    umm :: Proxy s -> (f (ReflectedEq a s) -> f (ReflectedEq a s) -> Bool) -> f a -> f a -> Bool
    umm _ g a' b' = g (coerce1 a') (coerce1 b')

-- | Since Eq1 has a strange type signature, we can use this definition
--   since 'liftingEq' isn't general enough. Note that this is __NOT__
--   always a valid definition for eq1. (e.g. @data A = A (Maybe a)
--   (Maybe b) would give an incorrect definition.)@
foldableEq :: Foldable f => (a -> b -> Bool) -> f a -> f b -> Bool
foldableEq f a b = and $ zipWith f (toList a) (toList b)

-- Ord -----------------------------------------------------------------

newtype ReifiedOrd a = ReifiedOrd { runOrd :: a -> a -> Ordering }

newtype ReflectedOrd a s = ReflectedOrd a

instance Reifies s (ReifiedOrd a) => Eq (ReflectedOrd a s) where
  (==) = ( (==EQ).) . coerce (reflect (Proxy :: Proxy s))

instance Reifies s (ReifiedOrd a) => Ord (ReflectedOrd a s) where
  compare = coerce (reflect (Proxy :: Proxy s))

liftingOrd :: forall f a. (Coercible1 f, Lifting Ord f) => (a -> a -> Ordering) -> f a -> f a -> Ordering
liftingOrd f a b = reify (ReifiedOrd f) (\p -> umm p compare a b \\ subsOrd p)
  where
    subsOrd :: Proxy s -> Ord (ReflectedOrd a s) :- Ord (f (ReflectedOrd a s))
    subsOrd _ = lifting

    umm :: Proxy s -> (f (ReflectedOrd a s) -> f (ReflectedOrd a s) -> Ordering) -> f a -> f a -> Ordering
    umm _ g a' b' = g (coerce1 a') (coerce1 b')

-- liftingReadsPrec :: Lifting Read f => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
-- -- liftingReadList :: Lifting Read f => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)

-- Show ----------------------------------------------------------------

data ReifiedShow a = ReifiedShow { runShowsPrec :: Int -> a -> ShowS, runShowList :: [a] -> ShowS }

newtype ReflectedShow a s = ReflectedShow a

instance Reifies s (ReifiedShow a) => Show (ReflectedShow a s) where
  showsPrec = coerce $ runShowsPrec (reflect (Proxy :: Proxy s))
  showList = coerce $ runShowList (reflect (Proxy :: Proxy s))

subsShow :: Lifting Show f => Proxy s -> f a -> Show (ReflectedShow a s) :- Show (f (ReflectedShow a s))
subsShow _ _ = lifting

refShow1 :: Coercible1 f => Proxy s -> f a -> f (ReflectedShow a s)
refShow1 _ = coerce1

-- | This can be used as a definition of 'liftShowsPrec' in a 'Show1'
--   instance.
liftingShowsPrec :: (Coercible1 f, Lifting Show f) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
liftingShowsPrec s1 s2 i fa = reify (ReifiedShow s1 s2) (\p -> showsPrec i (refShow1 p fa) \\ subsShow p fa)

-- | This can be used as a definition of 'liftShowList' in a 'Show1'
--   instance.
liftingShowList :: (Coercible1 f, Lifting Show f) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
liftingShowList s1 s2 fa = reify (ReifiedShow s1 s2) (\p -> showList (refShowL1 p fa) \\ subsShowL p fa)
  where
    subsShowL :: Lifting Show f => Proxy s -> [f a] -> Show (ReflectedShow a s) :- Show (f (ReflectedShow a s))
    subsShowL _ _ = lifting

    refShowL1 :: Coercible1 f => Proxy s -> [f a] -> [f (ReflectedShow a s)]
    refShowL1 _ = fmap coerce1

-- Example -------------------------------------------------------------

-- data ComplicatedType a = C [a] (Maybe a) (Either Char a)
--   deriving Show

-- instance Coercible1 ComplicatedType where coercion1 = Coercion
-- instance Lifting Show ComplicatedType where lifting = Sub Dict

-- instance Show1 ComplicatedType where
--   liftShowsPrec = liftingShowsPrec
--   liftShowList = liftingShowList

-- data T f a = T (f (f a)) (f a)
--   -- deriving Show

-- coercionT :: forall f a b. (Coercible1 f, Coercible a b) => Coercion (T f a) (T f b)
-- coercionT = case coercion1 :: Coercion (f a) (f b) of
--   Coercion -> case coercion1 :: Coercion (f (f a)) (f (f b)) of
--     c -> unsafeCoerce c -- XXX how do I write this?

-- liftingOf :: Lifting p f => Proxy (p (f a)) -> p a :- p (f a)
-- liftingOf _ = lifting

-- instance Coercible1 f => Coercible1 (T f) where coercion1 = coercionT

-- instance (Lifting Show f, Show a) => Show (T f a) where
--   showsPrec d (T m v) = showParen (d > 10) $
--     showString "T " . showsPrec 11 m . showsPrec 11 v
--       \\ liftingOf (Proxy :: Proxy (Show (f (f a))))
--       \\ liftingOf (Proxy :: Proxy (Show (f a)))

-- instance Lifting Show f => Lifting Show (T f) where lifting = Sub Dict

-- instance (Coercible1 f, Lifting Show f) => Show1 (T f) where
--   liftShowsPrec = liftingShowsPrec
--   liftShowList = liftingShowList

