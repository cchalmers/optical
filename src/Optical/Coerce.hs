{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RoleAnnotations #-}
module Optical.Coerce
  ( Coercible1 (..)
  , coerce1
  , liftC

  , Coercible2 (..)
  , coerce2
  , lift2C

  , CoercedIso
  , CoercedIso'
  , coerced
  ) where

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

import Prelude hiding ((.), id)
import Control.Category

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

joinCoercion :: forall f g a. (Coercible1 f, Join f, Coercible1 g, Coercible f g) => Coercion (f (f a) -> f a) (g (g a) -> g a)
joinCoercion = contratrans (Coercion . (coercion1 :: Coercion (f (f a)) (f (g a)))) . coercion1

joinWith :: forall proxy f g a. (Coercible1 f, Join f, Coercible1 g, Coercible f g) => proxy f -> g (g a) -> g a
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

