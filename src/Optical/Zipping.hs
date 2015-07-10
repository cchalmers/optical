{-# LANGUAGE MultiParamTypeClasses #-}
module Optical.Zipping where

import Data.Functor.Apply
import Prelude hiding (zipWith, zipWith3)
import Control.Lens hiding ((<.>))
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

-- | Similar to 'Control.Monad.MonadZip' but only requires 'Apply' and
--   has a whole bunch of extra methods. This doesn't actually "need"
--   any instances as long as 'Apply' is defined in a way that safifies
--   the critera of "zipping" and union = zip (i.e for fixed size
--   containers like 'V3')

class Apply f => Zipping f where
  zipWith  :: (a -> b -> c) -> f a -> f b -> f c
  zipWith f a b = f <$> a <.> b
  zipWith3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  zipWith3 f a b c = f <$> a <.> b <.> c
  zipWith4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
  zipWith4 f a b c d = f <$> a <.> b <.> c <.> d
  zipWith5 :: (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
  zipWith5 f a b c d e = f <$> a <.> b <.> c <.> d <.> e
  zipWith6 :: (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
  zipWith6 f a b c d e g = f <$> a <.> b <.> c <.> d <.> e <.> g

  zip  :: f a -> f b -> f (a,b)
  zip = zipWith (,)
  zip3 :: f a -> f b -> f c -> f (a,b,c)
  zip3 = zipWith3 (,,)
  zip4 :: f a -> f b -> f c -> f d -> f (a,b,c,d)
  zip4 = zipWith4 (,,,)
  zip5 :: f a -> f b -> f c -> f d -> f e -> f (a,b,c,d,e)
  zip5 = zipWith5 (,,,,)
  zip6 :: f a -> f b -> f c -> f d -> f e -> f g -> f (a,b,c,d,e,g)
  zip6 = zipWith6 (,,,,,)

  zap  :: f (a -> b) -> f a -> f b
  zap = (<.>)
  zap3 :: f (a -> b -> c) -> f a -> f b -> f c
  zap3 = zipWith3 id
  zap4 :: f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  zap4 = zipWith4 id
  zap5 :: f (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
  zap5 = zipWith5 id
  zap6 :: f (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
  zap6 = zipWith6 id

  -- This doesn't nessesary belong here because it can be defined in
  -- terms of 'Functor' but it seems like a reasonable place to put it
  -- so containers can impliment more efficient implimentations.

  unzip  :: f (a, b) -> (f a, f b)
  unzip f = (get _1 f, get _2 f)
  unzip3 :: f (a, b, c) -> (f a, f b, f c)
  unzip3 f = (get _1 f, get _2 f, get _3 f)
  unzip4 :: f (a, b, c, d) -> (f a, f b, f c, f d)
  unzip4 f = (get _1 f, get _2 f, get _3 f, get _4 f)
  unzip5 :: f (a, b, c, d, e) -> (f a, f b, f c, f d, f e)
  unzip5 f = (get _1 f, get _2 f, get _3 f, get _4 f, get _5 f)
  unzip6 :: f (a, b, c, d, e, g) -> (f a, f b, f c, f d, f e, f g)
  unzip6 f = (get _1 f, get _2 f, get _3 f, get _4 f, get _5 f, get _6 f)

  -- unionWith  :: (a -> a -> a) -> f a -> f a -> f a
  -- unionWith = zipWith
  -- unionWith3 :: (a -> a -> a -> a) -> f a -> f a -> f a -> f a
  -- unionWith3 = zipWith3
  -- unionWith4 :: (a -> a -> a -> a -> a) -> f a -> f a -> f a -> f a -> f a
  -- unionWith4 = zipWith4
  -- unionWith5 :: (a -> a -> a -> a -> a -> a) -> f a -> f a -> f a -> f a -> f a -> f a
  -- unionWith5 = zipWith5
  -- unionWith6 :: (a -> a -> a -> a -> a -> a -> a) -> f a -> f a -> f a -> f a -> f a -> f a -> f a
  -- unionWith6 = zipWith6

get :: Functor f => Getting b s b -> f s -> f b
get l = fmap (view l)
{-# INLINE get #-}

instance Zipping [] where
  zipWith  = List.zipWith
  zipWith3 = List.zipWith3
  zipWith4 = List.zipWith4
  zipWith5 = List.zipWith5
  zipWith6 = List.zipWith6

  zip  = List.zip
  zip3 = List.zip3
  zip4 = List.zip4
  zip5 = List.zip5
  zip6 = List.zip6

  -- unionWith f = go where
  --   go (x:xs) (y:ys) = f x y : go xs ys
  --   go []     ys     = ys
  --   go xs     _      = xs

instance Zipping Seq.Seq where
  zipWith  = Seq.zipWith
  zipWith3 = Seq.zipWith3
  zipWith4 = Seq.zipWith4

  zip  = Seq.zip
  zip3 = Seq.zip3
  zip4 = Seq.zip4

class FunctorWithIndex i f => ZippingWithIndex i f where
  izipWith  :: (i -> a -> b -> c) -> f a -> f b -> f c
  -- izipWith f a b = f <$> a <.> b
  izipWith3 :: (i -> a -> b -> c -> d) -> f a -> f b -> f c -> f d
  -- izipWith3 f a b c = f <$> a <.> b <.> c
  izipWith4 :: (i -> a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
  -- izipWith4 f a b c d = f <$> a <.> b <.> c <.> d
  izipWith5 :: (i -> a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
  -- izipWith5 f a b c d e = f <$> a <.> b <.> c <.> d <.> e
  izipWith6 :: (i -> a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
  -- izipWith6 f a b c d e g = f <$> a <.> b <.> c <.> d <.> e <.> g

-- instance ZippingWithIndex Int Seq.Seq where
--   izipWith = Seq.zipWithIndex

instance ZippingWithIndex Int V.Vector where
  izipWith  = V.izipWith
  izipWith3 = V.izipWith3
  izipWith4 = V.izipWith4
  izipWith5 = V.izipWith5
  izipWith6 = V.izipWith6

