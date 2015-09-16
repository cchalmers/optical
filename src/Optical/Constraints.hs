{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Optical.Constraints where

-- import Linear
-- import Data.Aeson
import Data.Functor.Classes
import Data.Constraint
import Data.Constraint.Unsafe
import Data.Coerce
import Data.Semigroup
import Data.Functor.Bind
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

-- Wrapped internals ---------------------------------------------------

newtype Newtype a = Newtype a

instance Monoid a => Semigroup (Newtype a) where
  (<>) = coerce (mappend :: a -> a -> a)
  {-# INLINE (<>) #-}

newtype Wrapped1 f a = Wrapped1 (f a)

instance (Show1 f, Show a) => Show (Wrapped1 f a) where
  showsPrec = coerce (showsPrec1 :: Int -> f a -> ShowS)
  {-# INLINE showsPrec #-}

instance (Eq1 f, Eq a) => Eq (Wrapped1 f a) where
  (==) = coerce (eq1 :: f a -> f a -> Bool)
  {-# INLINE (==) #-}

instance (Ord1 f, Ord a) => Ord (Wrapped1 f a) where
  compare = coerce (compare1 :: f a -> f a -> Ordering)
  {-# INLINE compare #-}

instance (Read1 f, Read a) => Read (Wrapped1 f a) where
  readsPrec = coerce (readsPrec1 :: Int -> ReadS (f a))
  {-# INLINE readsPrec #-}

instance Functor f => Functor (Wrapped1 f) where
  fmap = (coerce :: ((a -> b) -> f a -> f b) -> (a -> b) -> Wrapped1 f a -> Wrapped1 f b) fmap
  {-# INLINE fmap #-}

instance Applicative f => Apply (Wrapped1 f) where
  (<.>) = (coerce :: (f (a -> b) -> f a -> f b) -> Wrapped1 f (a -> b) -> Wrapped1 f a -> Wrapped1 f b) (<*>)
  {-# INLINE (<.>) #-}
  (.>)  = (coerce :: (f a -> f b -> f b) -> Wrapped1 f a -> Wrapped1 f b -> Wrapped1 f b) (*>)
  {-# INLINE (.>) #-}
  (<.)  = (coerce :: (f a -> f b -> f a) -> Wrapped1 f a -> Wrapped1 f b -> Wrapped1 f a) (<*)
  {-# INLINE (<.) #-}

instance Monad m => Bind (Wrapped1 m) where
  (>>-) = (coerce :: (m a -> (a -> m b) -> m b) -> Wrapped1 m a -> (a -> Wrapped1 m b) -> Wrapped1 m b) (>>=)
  {-# INLINE (>>-) #-}

unsafeWrapped :: p (Wrapped1 f a) :- p (f a)
unsafeWrapped = unsafeCoerceConstraint
{-# INLINE unsafeWrapped #-}

unsafeWrapped1 :: p (Wrapped1 f) :- p f
unsafeWrapped1 = unsafeCoerceConstraint
{-# INLINE unsafeWrapped1 #-}

-- Constraints ---------------------------------------------------------

-- | Construct a 'Show' instance from 'Show1'.
unsafeShow :: Show a => Show1 f :- Show (f a)
unsafeShow = trans unsafeWrapped (Sub Dict)
{-# INLINE unsafeShow #-}

-- | Construct an 'Eq' instance from 'Eq1'.
unsafeEq :: Eq a => Eq1 f :- Eq (f a)
unsafeEq = trans unsafeWrapped (Sub Dict)
{-# INLINE unsafeEq #-}

-- | Construct an 'Ord instance from 'Ord1'.
unsafeOrd :: Ord a => Ord1 f :- Ord (f a)
unsafeOrd = trans unsafeWrapped (Sub Dict)
{-# INLINE unsafeOrd #-}

-- | Construct a 'Read' instance from 'Read1'.
unsafeRead :: Read a => Read1 f :- Read (f a)
unsafeRead = trans unsafeWrapped (Sub Dict)
{-# INLINE unsafeRead #-}

-- | Construct an 'Apply' instance from an 'Applicative'.
unsafeApply :: Applicative f :- Apply f
unsafeApply = trans unsafeWrapped1 (Sub Dict)
{-# INLINE unsafeApply #-}

-- | Construct a 'Bind' instance from a 'Monad'.
unsafeBind :: Monad m :- Bind m
unsafeBind = trans unsafeWrapped1 (Sub Dict)
{-# INLINE unsafeBind #-}

-- | Construct a 'Semigroup' instance from a 'Monoid.
unsafeSemigroup :: Monoid a :- Semigroup a
unsafeSemigroup = trans (unsafeCoerceConstraint :: Semigroup (Newtype a) :- Semigroup a) (Sub Dict)
{-# INLINE unsafeSemigroup #-}

-- unsafeRepApply :: Representable f :- Apply f
-- unsafeRepApplicative :: Representable f :- Applicative f
-- unsafeRepBind :: Representable f :- Bind f
-- unsafeRepMonad :: Representable f :- Monad f
-- unsafeRepMonadZip :: Representable f :- MonadZip f

-- unsafeProfunctorFunctor :: Profunctor p :- Functor (p a)
-- unsafeBiFunctorFunctor :: BiFunctor p :- Functor (p a)

-- JSON ----------------------------------------------------------------

instance (Generic a, GToJSON (Rep a)) => ToJSON (Newtype a) where
  toJSON = coerce (genericToJSON defaultOptions :: a -> Value)
  {-# INLINE toJSON #-}

instance (Generic a, GFromJSON (Rep a)) => FromJSON (Newtype a) where
  parseJSON = coerce (genericParseJSON defaultOptions :: Value -> Parser a)
  {-# INLINE parseJSON #-}

unsafeToJSON :: (GToJSON (Rep a), Generic a) :- ToJSON a
unsafeToJSON = trans (unsafeCoerceConstraint :: ToJSON (Newtype a) :- ToJSON a) (Sub Dict)

unsafeFromJSON :: (GFromJSON (Rep a), Generic a) :- FromJSON a
unsafeFromJSON = trans (unsafeCoerceConstraint :: FromJSON (Newtype a) :- FromJSON a) (Sub Dict)

unsafeJSON :: forall a. (GToJSON (Rep a), GFromJSON (Rep a), Generic a) :- (ToJSON a, FromJSON a)
unsafeJSON = Sub $ Dict \\ (unsafeToJSON :: (GToJSON (Rep a), Generic a) :- ToJSON a)
                        \\ (unsafeFromJSON :: (GFromJSON (Rep a), Generic a) :- FromJSON a)

unsafeJSONFor :: a -> (GToJSON (Rep a), GFromJSON (Rep a), Generic a) :- (ToJSON a, FromJSON a)
unsafeJSONFor _ = unsafeJSON

