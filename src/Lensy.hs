{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Lensy
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
  , P.break
  , P.cycle
  , P.drop
  , P.dropWhile
  , P.filter
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
  , P.span
  , P.splitAt
  , tail
  , P.take
  , P.takeWhile
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
  , P.traverse
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
  , P.Traversable

  -- data types
  , P.IO
  , P.Char
  , P.Double
  , P.Float
  , P.Int
  , P.Integer
  , P.Word
  , P.Bool (True, False)
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


  -- | Isos and contructors for common containers.
  , module Lensy.Containers
  , module Control.Monad.Primitive

  ) where

import Prelude.Compat hiding ((++), (!!), (^), head, tail, last, init, map, reverse, lookup)
import qualified Prelude.Compat as P
import Control.Lens hiding (mapOf)

import Data.Semigroup
import Control.Monad.Primitive
import GHC.Generics
import Control.Monad.State
import Data.Distributive
import Data.Foldable as F

import Data.Text.Lens (IsText (text))
import Data.ByteString.Lens (packedBytes)
import Data.Vector.Generic.Lens
import Data.Vector.Generic.Lens
import Numeric.Lens
import System.Exit.Lens
import System.FilePath.Lens
import System.IO.Error.Lens
import Data.Tree.Lens
import Data.Set.Lens (setOf, setmapped)
import qualified Data.HashSet.Lens as HashSet
import Data.Word
import Data.Hashable
import qualified Data.List as List

import Lensy.Containers

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

-- | Check is the item is empty.
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
