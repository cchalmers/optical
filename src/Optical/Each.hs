{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Optical.Each where

import Control.Lens -- hiding (coerce)
-- import Control.Lens.Internal
-- import Data.Profunctor.Unsafe
-- import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lens
import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Lens as LBS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.HashMap.Strict as HM
import Data.Vector.Generic.Lens
import Data.Int
import Data.Hashable
import Data.Tree
-- import Control.Applicative.Backwards

-- These aren't proper "Traversals" in the they don't satisfy the
-- traversal laws. But I see the 'Each' class as a convient class for
-- overloading the 'each' name.

-- instance Each IntSet.IntSet IntSet.IntSet Int Int where
--   each f = IntSet.fromList . traverse f . IntSet.toList

-- instance (Ord a, Ord b) => Each Set.Set Set.Set a b where
--   each f = IntSet.fromList . traverse f . IntSet.toList

-- instance (Hashable a, Eq a, Hashable b, Eq b) => Each HashSet.HashSet HashSet.HashSet a b where
--   each f = IntSet.fromList . traverse f . IntSet.toList

-- | Like 'traverse_' but for 'Each'.
each_ :: (Applicative f, Each s s b b) => (b -> f r) -> s -> f ()
each_ = traverseOf_ each

class Each s t a b => EachWithIndex s t a b where
  ieach :: IndexedTraversal (Index s) s t a b

instance EachWithIndex (B.Vector a) (B.Vector b) a b where
  ieach = vectorTraverse
  {-# INLINE ieach #-}

instance (U.Unbox a, U.Unbox b) => EachWithIndex (U.Vector a) (U.Vector b) a b where
  ieach = vectorTraverse
  {-# INLINE ieach #-}

instance (S.Storable a, S.Storable b) => EachWithIndex (S.Vector a) (S.Vector b) a b where
  ieach = vectorTraverse
  {-# INLINE ieach #-}

instance (P.Prim a, P.Prim b) => EachWithIndex (P.Vector a) (P.Vector b) a b where
  ieach = vectorTraverse
  {-# INLINE ieach #-}

instance EachWithIndex (S.Seq a) (S.Seq b) a b where
  ieach = traversed
  {-# INLINE ieach #-}

instance EachWithIndex [a] [b] a b where
  ieach = traversed
  {-# INLINE ieach #-}

instance (a ~ Word8, b ~ Word8) => EachWithIndex BS.ByteString BS.ByteString a b where
  ieach = bytes
  {-# INLINE ieach #-}

instance (a ~ Word8, b ~ Word8) => EachWithIndex LBS.ByteString LBS.ByteString a b where
  ieach = LBS.bytes
  {-# INLINE ieach #-}

instance (a ~ Char, b ~ Char) => EachWithIndex T.Text T.Text a b where
  ieach = text
  {-# INLINE ieach #-}

instance (a ~ Char, b ~ Char) => EachWithIndex LT.Text LT.Text a b where
  ieach = reindexed (fromIntegral :: Int -> Int64) text
  {-# INLINE ieach #-}

instance EachWithIndex (M.Map k a) (M.Map k b) a b where
  ieach = itraversed
  {-# INLINE ieach #-}

instance EachWithIndex (IM.IntMap a) (IM.IntMap b) a b where
  ieach = itraversed
  {-# INLINE ieach #-}

instance (Eq k, Hashable k) => EachWithIndex (HM.HashMap k a) (HM.HashMap k b) a b where
  ieach = itraversed
  {-# INLINE ieach #-}

instance EachWithIndex (Tree a) (Tree b) a b where
  ieach = itraversed
  {-# INLINE ieach #-}

-- | Like 'itraverse_' but for 'EachWithIndex'.
ieach_ :: (Applicative f, EachWithIndex s s b b) => (Index s -> b -> f r) -> s -> f ()
ieach_ = itraverseOf_ ieach
{-# INLINE ieach_ #-}

