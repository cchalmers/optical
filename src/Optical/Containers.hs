{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

------------------------------------------------------------------------
-- Isos and contructors for various containers.
------------------------------------------------------------------------

module Optical.Containers
  (
  -- ** vectors
    GVector
  , _GVector
  , toGVector
  , toGVectorOf
  , itoGVector
  , itoGVectorOf

  , BVector
  , _BVector
  , toBVector
  , toBVectorOf
  , itoBVector
  , itoBVectorOf

  , UVector
  , _UVector
  , toUVector
  , toUVectorOf
  , itoUVector
  , itoUVectorOf

  , SVector
  , _SVector
  , toSVector
  , toSVectorOf

  , PVector
  , _PVector
  , toPVector
  , toPVectorOf

  , byteVector

  -- * Containers
  -- ** Seq
  , Seq.Seq
  , _Seq
  , toSeq
  , toSeqOf
  , itoSeq
  , itoSeqOf

  -- ** Map
  , Map.Map
  , _Map
  , toMap
  , toMapOf

  -- ** IntMap
  , IntMap.IntMap
  , _IntMap
  , toIntMap
  , toIntMapOf

  -- ** HashMap
  , HashMap.HashMap
  , _HashMap
  , toHashMap
  , toHashMapOf

  -- ** HashSet
  , HashSet.HashSet
  , _HashSet
  , toHashSet
  , toHashSetOf

  -- ** Set
  , Set.Set
  , _Set
  , toSet
  , toSetOf

  -- ** IntSet
  , IntSet.IntSet
  , _IntSet
  , toIntSet
  , toIntSetOf

  -- ** Text
  , Text.Text
  , _Text
  , toText
  , toTextOf

  -- ** Lazy text
  , LText
  , _LText
  , toLText
  , toLTextOf

  -- ** Byte string
  , BS.ByteString
  , _ByteString
  , toByteString
  , toByteStringOf

  -- ** Lazy byte string
  , LByteString
  , _LByteString
  , toLByteString
  , toLByteStringOf
  ) where

import Control.Lens
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as PV
-- import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as TBuilder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.Monoid
import Data.Word
import Data.Foldable (toList)
import           Data.ByteString.Internal (ByteString (PS))
import           Data.Vector.Storable     (unsafeFromForeignPtr,
                                           unsafeToForeignPtr)
import Prelude

-- Vectors -------------------------------------------------------------

-- | Generic vector constraint.
type GVector = G.Vector

_GVector :: (GVector v a, GVector w b) => Iso (v a) (w b) [a] [b]
_GVector = iso G.toList G.fromList
{-# INLINE _GVector #-}

_GVector' :: GVector v a => Iso' (v a) [a]
_GVector' = _GVector
{-# INLINE _GVector' #-}

toGVector :: (Foldable f, GVector v a) => f a -> v a
toGVector = G.fromList . toList
{-# INLINE toGVector #-}

toGVectorOf :: GVector v a => Getting (Endo [a]) s a -> s -> v a
toGVectorOf l = G.fromList . toListOf l
{-# INLINE toGVectorOf #-}

itoGVector :: (FoldableWithIndex i f, GVector v (i, a)) => f a -> v (i, a)
itoGVector = G.fromList . itoList
{-# INLINE itoGVector #-}

itoGVectorOf :: GVector v (i, a) => IndexedGetting i (Endo [(i,a)]) s a -> s -> v (i,a)
itoGVectorOf l = G.fromList . itoListOf l
{-# INLINE itoGVectorOf #-}

-- | Boxed vector.
type BVector = B.Vector

_BVector :: Iso (BVector a) (BVector b) [a] [b]
_BVector = _GVector
{-# INLINE _BVector #-}

toBVector :: Foldable f => f a -> BVector a
toBVector = G.fromList . toList
{-# INLINE toBVector #-}

toBVectorOf :: Getting (Endo [a]) s a -> s -> BVector a
toBVectorOf l = G.fromList . toListOf l
{-# INLINE toBVectorOf #-}

itoBVector :: FoldableWithIndex i f => f a -> BVector (i, a)
itoBVector = G.fromList . itoList
{-# INLINE itoBVector #-}

itoBVectorOf :: IndexedGetting i (Endo [(i,a)]) s a -> s -> BVector (i,a)
itoBVectorOf l = G.fromList . itoListOf l
{-# INLINE itoBVectorOf #-}

-- | 'Unboxed' vector.
type UVector = U.Vector

_UVector :: (U.Unbox a, U.Unbox b) => Iso (UVector a) (UVector b) [a] [b]
_UVector = _GVector
{-# INLINE _UVector #-}

toUVector :: (Foldable f, U.Unbox a) => f a -> UVector a
toUVector = G.fromList . toList
{-# INLINE toUVector #-}

toUVectorOf :: U.Unbox a => Getting (Endo [a]) s a -> s -> UVector a
toUVectorOf l = G.fromList . toListOf l
{-# INLINE toUVectorOf #-}

itoUVector :: (U.Unbox i, U.Unbox a, FoldableWithIndex i f) => f a -> UVector (i, a)
itoUVector = G.fromList . itoList
{-# INLINE itoUVector #-}

itoUVectorOf :: (U.Unbox i, U.Unbox a) => IndexedGetting i (Endo [(i,a)]) s a -> s -> UVector (i,a)
itoUVectorOf l = G.fromList . itoListOf l
{-# INLINE itoUVectorOf #-}

-- | 'Storable' vector.
type SVector = S.Vector

_SVector :: (S.Storable a, S.Storable b) => Iso (SVector a) (SVector b) [a] [b]
_SVector = _GVector
{-# INLINE _SVector #-}

toSVector :: (Foldable f, S.Storable a) => f a -> SVector a
toSVector = G.fromList . toList
{-# INLINE toSVector #-}

toSVectorOf :: S.Storable a => Getting (Endo [a]) s a -> s -> SVector a
toSVectorOf l = G.fromList . toListOf l
{-# INLINE toSVectorOf #-}

-- | 'Prim'itive vector.
type PVector = PV.Vector

_PVector :: (PV.Prim a, PV.Prim b) => Iso (PVector a) (PVector b) [a] [b]
_PVector = _GVector
{-# INLINE _PVector #-}

toPVector :: (Foldable f, PV.Prim a) => f a -> PVector a
toPVector = G.fromList . toList
{-# INLINE toPVector #-}

toPVectorOf :: PV.Prim a => Getting (Endo [a]) s a -> s -> PVector a
toPVectorOf l = G.fromList . toListOf l
{-# INLINE toPVectorOf #-}

-- | O(1) isomorphism between a 'ByteString' and a vector of 'Word8'.
byteVector :: Iso' ByteString (SVector Word8)
byteVector = iso toByteVector fromByteVector
  where
  toByteVector (PS fptr offset idx) =
    unsafeFromForeignPtr fptr offset idx

  fromByteVector v = PS fptr offset idx
    where (fptr, offset, idx) = unsafeToForeignPtr v
{-# INLINE byteVector #-}

-- primStored :: (PV.Prim a, S.Storable a) => Prism' (PVector a) (SVector a)
-- primStored = prism' toPrim fromPrim
--   where
--     toPrim

-- data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents

-- data ForeignPtrContents = PlainForeignPtr !(IORef (Finalizers, [IO ()]))
--                         | MallocPtr      (MutableByteArray# RealWorld) !(IORef (Finalizers, [IO ()]))
--                         | PlainPtr       (MutableByteArray# RealWorld)

-- toPtr :: MutableByteArray -> ForeignPtr


-- Sequences -----------------------------------------------------------

_Seq :: Iso (Seq.Seq a) (Seq.Seq b) [a] [b]
_Seq = _Wrapped
{-# INLINE _Seq #-}

toSeq :: Foldable f => f a -> Seq.Seq a
toSeq = toSeqOf folded
{-# INLINE toSeq #-}

toSeqOf :: Getting (Seq.Seq a) s a -> s -> Seq.Seq a
toSeqOf l = views l Seq.singleton
{-# INLINE toSeqOf #-}

itoSeq :: FoldableWithIndex i f => f a -> Seq.Seq (i,a)
itoSeq = itoSeqOf ifolded
{-# INLINE itoSeq #-}

itoSeqOf :: IndexedGetting i (Seq.Seq (i,a)) s a -> s -> Seq.Seq (i,a)
itoSeqOf l = iviews l (\i a -> Seq.singleton (i,a))
{-# INLINE itoSeqOf #-}

-- Maps ----------------------------------------------------------------

_Map :: (Ord k, Ord l) => Iso (Map.Map k a) (Map.Map l b) [(k,a)] [(l,b)]
_Map = _Wrapped
{-# INLINE _Map #-}

toMap :: Ord k => FoldableWithIndex k f => f a -> Map.Map k a
toMap = toMapOf ifolded
{-# INLINE toMap #-}

toMapOf :: IndexedGetting k (Map.Map k a) s a -> s -> Map.Map k a
toMapOf l = iviews l Map.singleton
{-# INLINE toMapOf #-}

_IntMap :: Iso (IntMap.IntMap a) (IntMap.IntMap b) [(Int,a)] [(Int,b)]
_IntMap = _Wrapped
{-# INLINE _IntMap #-}

toIntMap :: FoldableWithIndex Int f => f a -> IntMap.IntMap a
toIntMap = toIntMapOf folded
{-# INLINE toIntMap #-}

toIntMapOf :: IndexedGetting Int (IntMap.IntMap a) s a -> s -> IntMap.IntMap a
toIntMapOf l = iviews l IntMap.singleton
{-# INLINE toIntMapOf #-}

_HashMap :: (Hashable k, Eq k, Hashable l, Eq l) => Iso (HashMap.HashMap k a) (HashMap.HashMap l b) [(k,a)] [(l,b)]
_HashMap = _Wrapped
{-# INLINE _HashMap #-}

toHashMap :: (FoldableWithIndex k f, Hashable k, Eq k) => f a -> HashMap.HashMap k a
toHashMap = toHashMapOf ifolded
{-# INLINE toHashMap #-}

toHashMapOf :: Hashable k => IndexedGetting k (HashMap.HashMap k a) s a -> s -> HashMap.HashMap k a
toHashMapOf l = iviews l HashMap.singleton
{-# INLINE toHashMapOf #-}

-- Sets ----------------------------------------------------------------

_IntSet :: Iso (IntSet.IntSet) (IntSet.IntSet) [Int] [Int]
_IntSet = _Wrapped
{-# INLINE _IntSet #-}

toIntSet :: Foldable f => f Int -> IntSet.IntSet
toIntSet = toIntSetOf folded
{-# INLINE toIntSet #-}

toIntSetOf :: Getting IntSet.IntSet s Int -> s -> IntSet.IntSet
toIntSetOf l = views l IntSet.singleton
{-# INLINE toIntSetOf #-}

_Set :: (Ord a, Ord b) => Iso (Set.Set a) (Set.Set b) [a] [b]
_Set = _Wrapped
{-# INLINE _Set #-}

toSet :: (Foldable f, Ord a) => f a -> Set.Set a
toSet = toSetOf folded
{-# INLINE toSet #-}

toSetOf :: Getting (Set.Set a) s a -> s -> Set.Set a
toSetOf l = views l Set.singleton
{-# INLINE toSetOf #-}

_HashSet :: (Hashable a, Eq a, Hashable b, Eq b) => Iso (HashSet.HashSet a) (HashSet.HashSet b) [a] [b]
_HashSet = _Wrapped
{-# INLINE _HashSet #-}

toHashSet :: (Foldable f, Hashable a, Eq a) => f a -> HashSet.HashSet a
toHashSet = toHashSetOf folded
{-# INLINE toHashSet #-}

toHashSetOf :: Hashable a => Getting (HashSet.HashSet a) s a -> s -> HashSet.HashSet a
toHashSetOf l = views l HashSet.singleton
{-# INLINE toHashSetOf #-}

-- Text ----------------------------------------------------------------

_Text :: Iso' Text.Text String
_Text = iso Text.unpack Text.pack
{-# INLINE _Text #-}

toText :: Foldable f => f Char -> Text.Text
toText = toTextOf folded
{-# INLINE toText #-}

toTextOf :: Getting (Endo [Char]) s Char -> s -> Text.Text
toTextOf l = Text.pack . toListOf l
{-# INLINE toTextOf #-}

_LText :: Iso' LText String
_LText = iso LText.unpack LText.pack
{-# INLINE _LText #-}

toLText :: Foldable f => f Char -> LText
toLText = toLTextOf folded
{-# INLINE toLText #-}

-- Is this better then @pack . toListOf l@?
toLTextOf :: Getting TBuilder.Builder s Char -> s -> LText
toLTextOf l = TBuilder.toLazyText . views l TBuilder.singleton
{-# INLINE toLTextOf #-}

_ByteString :: Iso' BS.ByteString [Word8]
_ByteString = iso BS.unpack BS.pack
{-# INLINE _ByteString #-}

toByteString :: Foldable f => f Word8 -> BS.ByteString
toByteString = toByteStringOf folded
{-# INLINE toByteString #-}

toByteStringOf :: Getting (Endo [Word8]) s Word8 -> s -> BS.ByteString
toByteStringOf l = BS.pack . toListOf l
{-# INLINE toByteStringOf #-}

_LByteString :: Iso' LByteString [Word8]
_LByteString = iso LBS.unpack LBS.pack
{-# INLINE _LByteString #-}

toLByteString :: Foldable f => f Word8 -> LByteString
toLByteString = toLByteStringOf folded
{-# INLINE toLByteString #-}

toLByteStringOf :: Getting (Endo [Word8]) s Word8 -> s -> LByteString
toLByteStringOf l = LBS.pack . toListOf l
{-# INLINE toLByteStringOf #-}
-- Vectors -------------------------------------------------------------

-- | Lazy chunks of strict text.
type LText = LText.Text

-- | Lazy chunks of strict byte strings.
type LByteString = LBS.ByteString
