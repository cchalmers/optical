{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Optical
  ( P.either
  , P.all
  , P.and
  , P.any
  , P.concat
  , P.concatMap
  , P.mapM_
  , P.notElem
  , P.or
  , P.sequence_
  , (P.<$>)
  , P.maybe
  , P.lines
  , P.unlines
  , P.unwords
  , P.words
  , P.curry
  , P.fst
  , P.snd
  , P.uncurry
  , (P.$!)
  , (++)
  , (P..)
  , (P.=<<)
  , P.asTypeOf
  , P.const
  , P.flip
  , P.id
  , map
  , P.otherwise
  , P.until
  , P.ioError
  , P.userError
  , (!!?)
  , (!!)
  , (!?)
  , (!)
  -- , P.break
  , P.cycle
  -- , P.drop
  -- , P.dropWhile
  -- , P.filter
  , head
  , init
  , P.iterate
  , last
  , P.repeat
  , P.replicate
  , P.scanl
  , P.scanl1
  , P.scanr
  , P.scanr1
  -- , P.span
  -- , P.splitAt
  , tail
  -- , P.take
  -- , P.takeWhile
  , P.unzip
  , P.unzip3
  , P.zip
  , P.zip3
  , P.zipWith
  , P.zipWith3
  , P.subtract
  , P.lex
  , P.readParen
  , (^)
  , (P.^^)

  , P.even
  , P.fromIntegral
  , P.gcd
  , P.lcm
  , P.odd
  , P.realToFrac
  , P.showChar
  , P.showParen
  , P.showString
  , P.shows
  , P.appendFile
  , P.getChar
  , P.getContents
  , P.getLine
  , P.interact
  , P.print
  , P.putChar
  , P.putStr
  , P.putStrLn
  , P.readFile
  , P.readIO
  , P.readLn
  , P.writeFile
  , P.read
  , P.reads
  , (P.&&)
  , P.not
  , (P.||)
  , (P.$)
  , P.error
  , P.undefined
  , P.seq

  , P.elem
  , P.foldMap
  , P.foldl
  , F.foldl'
  , P.foldl1
  , P.foldr
  , P.foldr1
  , P.length
  , P.maximum
  , P.minimum
  , P.null
  , empty
  , isEmpty
  , transpose
  , P.product
  , P.sum
  , P.mapM
  , P.sequence
  , P.sequenceA
  -- , P.traverse
  , (P.*>)
  , (P.<*)
  , (P.<*>)
  , P.pure
  , (P.<$)
  , P.fmap
  , (P.>>)
  , (P.>>=)
  , P.fail
  , P.return
  , P.mappend
  , P.mconcat
  , P.mempty
  , P.maxBound
  , P.minBound
  , P.enumFrom
  , P.enumFromThen
  , P.enumFromThenTo
  , P.enumFromTo
  , P.fromEnum
  , P.pred
  , P.succ
  , P.toEnum
  , (P.**)
  , P.acos
  , P.acosh
  , P.asin
  , P.asinh
  , P.atan
  , P.atanh
  , P.cos
  , P.cosh
  , P.exp
  , P.log
  , P.logBase
  , P.pi
  , P.sin
  , P.sinh
  , P.sqrt
  , P.tan
  , P.tanh
  , P.atan2
  , P.decodeFloat
  , P.encodeFloat
  , P.exponent
  , P.floatDigits
  , P.floatRadix
  , P.floatRange
  , P.isDenormalized
  , P.isIEEE
  , P.isInfinite
  , P.isNaN
  , P.isNegativeZero
  , P.scaleFloat
  , P.significand
  , (P.*)
  , (P.+)
  , (P.-)
  , P.abs
  , P.negate
  , P.signum
  , P.readList
  , P.readsPrec
  , (P./)
  , P.fromRational
  , P.recip
  , P.div
  , P.divMod
  , P.mod
  , P.quot
  , P.quotRem
  , P.rem
  , P.toInteger
  , P.toRational
  , P.ceiling
  , P.floor
  , P.properFraction
  , P.round
  , P.truncate
  , P.show
  , P.showList
  , P.showsPrec
  , (P./=)
  , (P.==)
  , (P.<)
  , (P.<=)
  , (P.>)
  , (P.>=)
  , P.compare
  , P.max
  , P.min

  -- lists

  , findIndex
  , findIndices
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , reverse
  , lookup
  , elemIndex
  , elemIndices
  , toMaybe

  -- classes
  , P.Applicative
  , P.Bounded
  , P.Enum
  , P.Eq
  , P.Floating
  , P.Foldable
  , P.Fractional
  , P.Functor
  , P.Integral
  , P.Monad
  , P.Monoid
  , P.Num (fromInteger)
  , P.Ord
  , P.Read
  , P.Real
  , P.RealFloat
  , P.RealFrac
  , P.Show
  -- , P.Traversable

  -- data types
  , P.IO
  , P.Char
  , P.Double
  , P.Float
  , P.Int
  , P.Integer
  , P.Word
  , P.Bool (True, False)
  , bool
  , P.Either(Left, Right)
  , P.Maybe(Just, Nothing)
  , P.Ordering (EQ, GT, LT)

  -- * type synonyms
  -- ** base
  , P.FilePath
  , P.IOError
  , P.Rational
  , P.ReadS
  , P.ShowS
  , P.String
  , Storable (..)

  , V0 (..)
  , V1 (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)

  -- * lensy

  , ijover
  , ijkover

  -- * Monads

  -- ** Transformers
  , MonadTrans (..)
  , MonadReader (..)
  -- , ReaderT
  -- , Reader
  -- , runReaderT
  -- , runReader

  , MonadState (..)
  -- , StateT
  -- , State
  -- , runStateT
  -- , runState

  , MonadWriter (..)
  -- , WriterT
  -- , Writer
  -- , runWriterT
  -- , runWriter

  , MonadIO (..)
  , PrimMonad

  -- * ST
  , ST
  , RealWorld
  , runST

  -- | Isos and contructors for common containers.
  , module Optical.Containers
  , module Optical.Text
  , module Optical.Witherable
  , module Optical.Severable
  , module Control.Lens
  , module Foreign
  , grab
  , Semigroup (..)

  ) where

import Prelude.Compat hiding ((++), (!!), (^), head, tail, last, init, map, reverse, lookup, filter)
import qualified Prelude.Compat as P
import Control.Lens hiding (mapOf, lined, worded)
import Linear (V0 (..), V1 (..), V2 (..), V3 (..), V4 (..))

import Control.Monad.Reader
import Foreign hiding (Int, Word)
import Control.Monad.State
import Control.Monad.Writer
import Data.Semigroup hiding (First)
import Data.Maybe
import Data.Bool
import Control.Monad.ST
import Foreign.Storable (Storable (..))
import Control.Monad.Primitive
import Data.Distributive
import Data.Foldable as F

import qualified Data.List as List

import Optical.Containers
import Optical.Text
import Optical.Witherable
import Optical.Severable

(^) :: Num a => a -> Int -> a
-- should really be word or natural
(^) = (P.^)

------------------------------------------------------------------------
-- Isos
------------------------------------------------------------------------

-- Arrays --------------------------------------------------------------

------------------------------------------------------------------------
-- Text-like
------------------------------------------------------------------------

-- class Monoid b => Building b t where
--   building :: Iso b t

--   buildOf :: Fold s Char -> s -> b

--   flush :: b

------------------------------------------------------------------------
-- Indexing
------------------------------------------------------------------------

-- | Partial index of an element. Throws an error if the element is out
--   of range.
(!?) :: Ixed a => a -> Index a -> Maybe (IxValue a)
x !? i = x ^? ix i

-- | Partial index of an element. Throws an error if the element is out
--   of range.
(!) :: Ixed a => a -> Index a -> IxValue a
x ! i = x ^?! ix i

-- | Index the ordinal position of an element.
(!!?) :: Foldable t => t a -> Int -> Maybe a
x !!? i = x ^? elementOf folded i

-- | Partial index the ordinal position of an element. Throws an error
--   if the element is out of range.
(!!) :: Foldable t => t a -> Int -> a
x !! i = x ^?! elementOf folded i

------------------------------------------------------------------------
-- List-like functions
------------------------------------------------------------------------

-- | Get the first element of a container if it exists.
head :: Cons s s a a => s -> Maybe a
head = preview _head

-- | Drop the first element of a container if it exists.
tail :: Cons s s a a => s -> Maybe s
tail = preview _tail

-- | Take the last element of a container if it exists.
last :: Snoc s s a a => s -> Maybe a
last = preview _last

-- | Drop the last element of a container if it exists.
init :: Snoc s s a a => s -> Maybe s
init = preview _init

-- | Check if the item is empty.
isEmpty :: AsEmpty a => a -> Bool
isEmpty = has _Empty

-- | The empty item.
empty :: AsEmpty a => a
empty = _Empty # ()

-- | Map over every element, using the 'each' class.
map :: Each s t a b => (a -> b) -> s -> t
map = over each

-- | Reverse something.
reverse :: Reversing a => a -> a
reverse = reversing

-- | Alias for 'distribute'.
transpose :: (Distributive g, Functor f) => f (g a) -> g (f a)
transpose = distribute

-- | Since 'Semigroup' is still not a superclass of 'Monoid' it's useful
--   to have separate functions.
(++) :: Monoid a => a -> a -> a
(++) = mappend

-- | Lookup at item in a folable container.
--
-- @
-- lookup :: 'Eq' i => i -> [(i,a)] -> 'Maybe' a
-- @
lookup :: (Eq i, Foldable f, FoldableWithIndex i g) => i -> f (g a) -> Maybe a
lookup i = preview (folded . ifolded . index i)

-- | Take the first item from a foldable if it exists.
toMaybe :: Foldable f => f a -> Maybe a
toMaybe = preview folded

elemIndex :: (FoldableWithIndex i f, Eq a) => a -> f a -> Maybe i
elemIndex = elemIndexOf ifolded

elemIndices :: (FoldableWithIndex i f, Eq a) => a -> f a -> [i]
elemIndices = elemIndicesOf ifolded

findIndex :: FoldableWithIndex i f => (a -> Bool) -> f a -> Maybe i
findIndex = findIndexOf ifolded

findIndices :: FoldableWithIndex i f => (a -> Bool) -> f a -> [i]
findIndices = findIndicesOf ifolded

isPrefixOf :: (Foldable f, Foldable g, Eq a) => f a -> g a -> Bool
isPrefixOf f g = F.toList f `List.isPrefixOf` F.toList g

isSuffixOf :: (Foldable f, Foldable g, Eq a) => f a -> g a -> Bool
isSuffixOf f g = F.toList f `List.isSuffixOf` F.toList g

isInfixOf :: (Foldable f, Foldable g, Eq a) => f a -> g a -> Bool
isInfixOf f g = F.toList f `List.isInfixOf` F.toList g

ijover :: AnIndexedSetter (V2 i) s t a b -> (i -> i -> a -> b) -> s -> t
ijover l f = iover l $ \(V2 i j) -> f i j

ijkover :: AnIndexedSetter (V3 i) s t a b -> (i -> i -> i -> a -> b) -> s -> t
ijkover l f = iover l $ \(V3 i j k) -> f i j k

-- | Grab the first result from a getter. If no result exist this will
--   throw an error.
grab :: MonadReader s m => Getting (First a) s a -> m a
grab l = fromMaybe (error "grab: empty getter") `liftM` preview l

-- Extas ---------------------------------------------------------------

-- | Only perform the action if the predicate returns 'True'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mbool action = mbool >>= flip when action

-- | Only perform the action if the predicate returns 'False'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action

-- -- | Throw a monadic exception from a String
-- --
-- -- > erroM = throwM . userError
-- errorM :: MonadThrow m => String -> m a
-- errorM = throwM . userError

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | Like 'whenJust', but where the test can be monadic.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

-- Data.List for Monad

-- | A version of 'partition' that works with a monadic predicate.
--
-- > partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
-- > partitionM (const Nothing) [1,2,3] == Nothing
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


-- -- | A version of 'concatMap' that works with a monadic predicate.
-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM f = liftM concat . mapM f

-- -- | A version of 'mapMaybe' that works with a monadic predicate.
-- mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
-- mapMaybeM f = liftM catMaybes . mapM f

-- Looping

-- | A looping operation, where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> return v

-- | Keep running an operation until it becomes 'False'. As an example:
--
-- @
-- whileM $ do sleep 0.1; notM $ doesFileExist "foo.txt"
-- readFile "foo.txt"
-- @
--
--   If you need some state persisted between each test, use 'loopM'.
whileM :: Monad m => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act

-- Booleans

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = do b <- mb; if b then t else f

-- | Like 'not', but where the test can be monadic.
notM :: Functor m => m Bool -> m Bool
notM = fmap not

-- | The lazy '||' operator lifted to a monad. If the first
--   argument evaluates to 'True' the second argument will not
--   be evaluated.
--
-- > Just True  ||^ undefined  == Just True
-- > Just False ||^ Just True  == Just True
-- > Just False ||^ Just False == Just False
(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b

-- | The lazy '&&' operator lifted to a monad. If the first
--   argument evaluates to 'False' the second argument will not
--   be evaluated.
--
-- > Just False &&^ undefined  == Just False
-- > Just True  &&^ Just True  == Just True
-- > Just True  &&^ Just False == Just False
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = ifM a b (return False)

-- module Data.List
--    (
--    -- * Basic functions

--    , intersperse -- ?
--    , intercalate -- ?

--    , subsequences -- ?
--    , permutations -- ?

--    -- ** Special folds

--    , concat -- Monoid
--    , concatMap -- Foldable / Monoid

--    -- * Building lists

--    -- ** Scans
--    , scanl -- ?
--    , scanl1 -- ?
--    , scanr -- ?
--    , scanr1 -- ?

--    -- ** Infinite lists
--    , iterate -- ?
--    , repeat -- ?
--    , replicate -- ?
--    , cycle -- ?

--    -- ** Unfolding
--    , unfoldr -- ?

--    -- * Sublists

--    -- ** Extracting sublists
--    , stripPrefix -- Witherable

--    , group

--    , inits
--    , tails

--    -- * Zipping and unzipping lists

--    , zip -- ?
--    , zip3 -- ?
--    , zip4, zip5, zip6, zip7 -- ?

--    , zipWith -- ?
--    , zipWith3 -- ?
--    , zipWith4, zipWith5, zipWith6, zipWith7 -- ?

--    , unzip -- ?
--    , unzip3 -- ?
--    , unzip4, unzip5, unzip6, unzip7 -- ?

--    -- * Special lists

--    -- ** Functions on strings
--    , lines -- ?
--    , words -- ?
--    , unlines -- ?
--    , unwords -- ?

--    -- ** \"Set\" operations

--    , union -- ?
--    , intersect -- ?

--    -- ** Ordered lists
--    , sort -- ?
--    , insert -- ?

--    -- *** User-supplied equality (replacing an @Eq@ context)
--    -- | The predicate is assumed to define an equivalence.
--    , nubBy -- Witherable
--    , deleteBy -- Witherable
--    , deleteFirstsBy -- Witherable
--    , unionBy -- ?
--    , intersectBy -- ?
--    , groupBy -- ?

--    -- *** User-supplied comparison (replacing an @Ord@ context)
--    -- | The function is assumed to define a total ordering.
--    , sortBy -- ?
--    , insertBy -- ?
