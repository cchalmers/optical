{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Optical.Patterns
  ( -- * Points
    pattern P0
  , pattern P1
  , pattern P2
  , pattern P3
  , pattern P4

#if __GLASGOW_HASKELL__ >= 709
    -- Numbers
  , pattern NaN
  , pattern Infinity
#endif
  ) where

import Linear.Affine
import Linear

pattern P0 :: Point V0 a
pattern P0 = P V0
pattern P1 :: a -> Point V1 a
pattern P1 x = P (V1 x)
pattern P2 :: a -> a -> Point V2 a
pattern P2 x y = P (V2 x y)
pattern P3 :: a -> a -> a -> Point V3 a
pattern P3 x y z = P (V3 x y z)
pattern P4 :: a -> a -> a -> a -> Point V4 a
pattern P4 x y z w = P (V4 x y z w)

#if __GLASGOW_HASKELL__ >= 709

#if __GLASGOW_HASKELL__ >= 800
pattern NaN :: RealFloat a => a
#else
pattern NaN :: () => RealFloat a => a
#endif
pattern NaN <- (isNaN -> True) where
  NaN = 0/0

#if __GLASGOW_HASKELL__ >= 800
pattern Infinity :: RealFloat a => a
#else
pattern Infinity :: () => RealFloat a => a
#endif
pattern Infinity <- (isInfinite -> True) where
  Infinity = 1/0
#endif

