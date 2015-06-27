{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Optical.Bit
  (
  -- * Bits
    Bit(..)
  , pattern On
  , pattern Off
  , _Bit

  -- * Bit Vectors with rank
  , BitVector(..)
  , _BitVector
  , rank
  , size
  , singleton

  -- * Vectors of Bits

  , UM.MVector(MV_Bit)
  , U.Vector(V_Bit)

  ) where

import           Control.Lens                as L
import           Control.Monad
import           Data.Bits
import           Data.Data
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Vector.Internal.Check  as Ck
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word
import           Prelude                     hiding (null)

#define BOUNDS_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Bounds)

-- | A simple newtype around a 'Bool'
--
-- The principal use of this is that a 'U.Vector' 'Bit' is densely
-- packed into individual bits rather than stored as one entry per 'Word8'.
newtype Bit = Bit { getBit :: Bool }
  deriving (Eq,Ord,Enum,Bounded,Data,Typeable)

instance Show Bit where
  show On = "On"
  show _  = "Off"

pattern On  = Bit True
pattern Off = Bit False

instance Rewrapped Bit Bit
instance Wrapped Bit where
  type Unwrapped Bit = Bool
  _Wrapped' = _Bit
  {-# INLINE _Wrapped' #-}

-- | 'Bit' and 'Bool' are isomorphic.
_Bit :: Iso' Bit Bool
_Bit = iso getBit Bit
{-# INLINE _Bit #-}

instance UM.Unbox Bit

data instance UM.MVector s Bit = MV_Bit {-# UNPACK #-} !Int {-# UNPACK #-} !(UM.MVector s Word64)

instance GM.MVector U.MVector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bit n _) = n
  basicUnsafeSlice i n (MV_Bit _ u) = MV_Bit n $ GM.basicUnsafeSlice i (wds n) u
  basicOverlaps (MV_Bit _ v1) (MV_Bit _ v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = do
    v <- GM.basicUnsafeNew (wds n)
    return $ MV_Bit n v
  basicUnsafeReplicate n (Bit b) = do
    v <- GM.basicUnsafeReplicate (wds n) (if b then -1 else 0)
    return $ MV_Bit n v
  basicUnsafeRead (MV_Bit _ u) i = do
    w <- GM.basicUnsafeRead u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeWrite (MV_Bit _ u) i (Bit b) = do
    let wn = wd i
    w <- GM.basicUnsafeRead u wn
    GM.basicUnsafeWrite u wn $ if b then setBit w (bt i) else clearBit w (bt i)
  basicClear (MV_Bit _ u) = GM.basicClear u
  basicSet (MV_Bit _ u) (Bit b) = GM.basicSet u $ if b then -1 else 0
  basicUnsafeCopy (MV_Bit _ u1) (MV_Bit _ u2) = GM.basicUnsafeCopy u1 u2
  basicUnsafeMove (MV_Bit _ u1) (MV_Bit _ u2) = GM.basicUnsafeMove u1 u2
  basicUnsafeGrow (MV_Bit _ u) n = liftM (MV_Bit n) (GM.basicUnsafeGrow u (wds n))

data instance U.Vector Bit = V_Bit {-# UNPACK #-} !Int {-# UNPACK #-} !(U.Vector Word64)
instance G.Vector U.Vector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicLength (V_Bit n _)          = n
  basicUnsafeFreeze (MV_Bit n u)   = liftM (V_Bit n) (G.basicUnsafeFreeze u)
  basicUnsafeThaw (V_Bit n u)      = liftM (MV_Bit n) (G.basicUnsafeThaw u)
  basicUnsafeSlice i n (V_Bit _ u) = V_Bit n (G.basicUnsafeSlice i (wds n) u)
  basicUnsafeIndexM (V_Bit _ u) i  = do
    w <- G.basicUnsafeIndexM u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeCopy (MV_Bit _ mu) (V_Bit _ u) = G.basicUnsafeCopy mu u
  elemseq _ b z = b `seq` z

#define BOUNDS_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Bounds)

-- | A BitVector support for naïve /O(1)/ 'rank'.
data BitVector = BitVector !(U.Vector Bit) {-# UNPACK #-} !(U.Vector Int)
  deriving (Eq,Ord,Show)

-- | /O(n) embedding/ A 'BitVector' is isomorphic to a vector of bits. It just carries extra information.
_BitVector :: Iso' BitVector (U.Vector Bit)
_BitVector = iso (\(BitVector v _) -> v) $ \v@(V_Bit _ ws) -> BitVector v $ G.scanl (\a b -> a + popCount b) 0 ws
{-# INLINE _BitVector #-}

-- | /O(1)/. @'rank' i v@ counts the number of 'True' bits up through and including the position @i@
rank :: BitVector -> Int -> Int
rank (BitVector (V_Bit n ws) ps) i
  = BOUNDS_CHECK(checkIndex) "rank" i n
  $ (ps U.! w) + popCount ((ws U.! w) .&. (bit (bt i + 1) - 1))
  where w = wd i
{-# INLINE rank #-}

instance AsEmpty BitVector where
  _Empty = nearly (_BitVector # G.empty) ((== 0) . size)

-- | /O(1)/. Return the size of the 'BitVector'.
size :: BitVector -> Int
size (BitVector (V_Bit n _) _) = n
{-# INLINE size #-}

type instance Index BitVector = Int
type instance IxValue BitVector = Bool
-- instance Ixed BitVector where
--   ix f (BitVector (V_Bit n v) r) =

{-
instance (Functor f, Contravariant f) => Contains f BitVector where
  contains i f (BitVector n as _) = coerce $ L.indexed f i (0 <= i && i < n && getBit (as U.! i))
-}

-- | Construct a 'BitVector' with a single element.
singleton :: Bool -> BitVector
singleton b = _BitVector # U.singleton (Bit b)
{-# INLINE singleton #-}

wds :: Int -> Int
wds x = unsafeShiftR (x + 63) 6
{-# INLINE wds #-}

wd :: Int -> Int
wd x = unsafeShiftR x 6
{-# INLINE wd #-}

bt :: Int -> Int
bt x = x .&. 63
{-# INLINE bt #-}

-- Maybe instance ------------------------------------------------------

instance UM.Unbox a => UM.Unbox (Maybe a)

data instance UM.MVector s (Maybe a) = MV_Maybe !(UM.MVector s Bit) !(UM.MVector s a)

instance U.Unbox a => GM.MVector U.MVector (Maybe a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Maybe b _) = GM.basicLength b
  basicUnsafeSlice i n (MV_Maybe b v) = MV_Maybe (GM.basicUnsafeSlice i n b) (GM.basicUnsafeSlice i n v)
  basicOverlaps (MV_Maybe b1 v1) (MV_Maybe b2 v2) = GM.basicOverlaps b1 b2 && GM.basicOverlaps v1 v2
  basicUnsafeNew n = do
    b <- GM.basicUnsafeNew n
    v <- GM.basicUnsafeNew n
    return $! MV_Maybe b v
  basicUnsafeReplicate n Nothing = do
    b <- GM.basicUnsafeReplicate n Off
    v <- GM.basicUnsafeNew n
    return $! MV_Maybe b v
  basicUnsafeReplicate n (Just a) = do
    b <- GM.basicUnsafeReplicate n On
    v <- GM.basicUnsafeReplicate n a
    return $! MV_Maybe b v
  basicUnsafeRead (MV_Maybe b v) i = do
    Bit e <- GM.basicUnsafeRead b i
    if e then Just `liftM` GM.basicUnsafeRead v i else return Nothing
  basicUnsafeWrite (MV_Maybe b _) i Nothing =
    GM.basicUnsafeWrite b i Off
  basicUnsafeWrite (MV_Maybe b v) i (Just a) = do
    GM.basicUnsafeWrite b i On
    GM.basicUnsafeWrite v i a
  basicClear (MV_Maybe b u) = GM.basicClear b >> GM.basicClear u
  basicSet (MV_Maybe b _) Nothing  = GM.basicSet b Off
  basicSet (MV_Maybe b v) (Just a) = do
    GM.basicSet b On
    GM.basicSet v a
  basicUnsafeCopy (MV_Maybe b1 v1) (MV_Maybe b2 v2) = do
    GM.basicUnsafeCopy b1 b2
    GM.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Maybe b1 v1) (MV_Maybe b2 v2) = do
    GM.basicUnsafeMove b1 b2
    GM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Maybe b v) n = do
    b' <- GM.basicUnsafeGrow b n
    v' <- GM.basicUnsafeGrow v n
    return $! MV_Maybe b' v'

data instance U.Vector (Maybe a) = V_Maybe !(U.Vector Bit) !(U.Vector a)
instance U.Unbox a => G.Vector U.Vector (Maybe a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicLength (V_Maybe b _)          = G.basicLength b
  basicUnsafeFreeze (MV_Maybe mb mv) = do
    b <- G.basicUnsafeFreeze mb
    v <- G.basicUnsafeFreeze mv
    return $! V_Maybe b v
  basicUnsafeThaw (V_Maybe b v)      = do
    mb <- G.basicUnsafeThaw b
    mv <- G.basicUnsafeThaw v
    return $! MV_Maybe mb mv
  basicUnsafeSlice i n (V_Maybe b v) = V_Maybe (G.basicUnsafeSlice i n b) (G.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_Maybe b v) i   = do
    Bit e <- G.basicUnsafeIndexM b i
    if e then Just `liftM` G.basicUnsafeIndexM v i else return Nothing
  basicUnsafeCopy (MV_Maybe mb mv) (V_Maybe b v) = do
    G.basicUnsafeCopy mb b
    G.basicUnsafeCopy mv v
  elemseq _ Nothing z = z
  elemseq _ (Just a) z = a `seq` z

