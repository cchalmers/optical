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
    -- * Cons / Snoc
  , pattern (:<)
  , pattern (:>)
  , pattern Empty
#endif
  ) where

import Control.Lens
import Control.Lens.Extras
import Linear.Affine
import Linear

pattern P0 = P V0
pattern P1 x = P (V1 x)
pattern P2 x y = P (V2 x y)
pattern P3 x y z = P (V3 x y z)
pattern P4 x y z w = P (V4 x y z w)

#if __GLASGOW_HASKELL__ >= 709
pattern x :< xs <- (uncons -> Just (x, xs)) where
  x :< xs = cons x xs

pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  xs :> x = snoc xs x

pattern Empty <- (is _Empty -> True) where
  Empty = _Empty # ()
#endif

