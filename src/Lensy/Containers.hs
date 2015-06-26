{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

------------------------------------------------------------------------
-- Isos and contructors for various containers.
------------------------------------------------------------------------

module Lensy.Containers
  (
  -- ** vectors
    BVector
  , UVector
  , SVector
  , PVector

  -- ** Containers
  , Seq.Seq
  , _Seq
  , toSeq
  , toSeqOf
  , itoSeq
  , itoSeqOf

  , Map.Map
  , _Map
  , toMap
  , toMapOf

  , IntMap.IntMap
  , _IntMap
  , toIntMap
  , toIntMapOf

  , HashMap.HashMap
  , _HashMap
  , toHashMap
  , toHashMapOf

  , HashSet.HashSet
  , _HashSet
  , toHashSet
  , toHashSetOf

  , Set.Set
  , _Set
  , toSet
  , toSetOf

  , IntSet.IntSet
  , _IntSet
  , toIntSet
  , toIntSetOf

  , Text.Text
  , _Text
  , toText
  , toTextOf

  , LText
  , _LText
  , toLText
  , toLTextOf

  , BS.ByteString
  , _ByteString
  , toByteString
  , toByteStringOf

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
import Data.Foldable (Foldable)
import Prelude

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

type LText = LText.Text

type LByteString = LBS.ByteString

type GVector = G.Vector
type BVector = B.Vector
type UVector = U.Vector
type SVector = S.Vector
type PVector = PV.Vector

