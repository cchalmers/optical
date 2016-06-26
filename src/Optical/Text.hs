{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FunctionalDependencies      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Optical.Text where

import           Optical.Containers

import           Control.Lens

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List
import qualified Data.Text            as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding   as LTE

import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8

import Prelude hiding (words, unwords, lines, unlines)

import Data.Monoid (Endo (..))
import Data.Profunctor.Unsafe

-- Chunked -------------------------------------------------------------

-- | Lazy constructs that are made of strict chunks.
class Strict l s => Chunked l s where
  -- | Isomorphism between a lazy verion and a list of strict versions.
  chunked :: Iso' l [s]

instance Chunked LT.Text T.Text where
  chunked = iso LT.toChunks LT.fromChunks

instance Chunked LBS.ByteString BS.ByteString where
  chunked = iso LBS.toChunks LBS.fromChunks

-- Text ----------------------------------------------------------------

class Textual t where
  -- | Indexed traversal over individual characters of something 'Textual'
  chars :: IndexedTraversal' Int t Char
  chars = string . traversed

  -- | Break a string up into a list of strings at newline characters.
  --   The resulting strings do not contain newlines.
  lines :: t -> [t]

  -- | Interspece strings with a newline char.
  unlines :: [t] -> t

  -- | Indexed traversal over the lines of a something 'Textual'. To be
  --   a valid traversal, each traversed line cannot add any extra
  --   newlines.
  lined :: IndexedTraversal' Int t t
  lined f = fmap unlines . traversed f . lines
  {-# INLINE lined #-}

  -- | Break a string up into a list of words, which were delimited by
  --   white space.
  words :: t -> [t]

  -- | Interspece strings with a space char.
  unwords :: [t] -> t

  -- | Indexed traversal over the words of a something 'Textual'. To be
  --   a valid traversal, each traversed word cannot add an extra spaces
  --   and the original text has isolated spaces.
  worded :: IndexedTraversal' Int t t
  worded f = fmap unwords . traversed f . words
  {-# INLINE worded #-}

  -- | Isomorphism between a textual and text.
  text :: Iso' t T.Text

  -- | Isomorphism between a textual and lazy text.
  lazyText :: Iso' t LT.Text

  -- | Isomorphism between a textual and utf8 encoded bytestring.
  utf8 :: Iso' t BS.ByteString

  -- | Isomorphism between a textual and utf8 encoded lazy bytestring.
  lazyUtf8 :: Iso' t LBS.ByteString

  -- | Isomorphism between a textual and string.
  string :: Iso' t String

instance Textual T.Text where
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE text #-}
  {-# INLINE lazyText #-}
  {-# INLINE string #-}
  {-# INLINE utf8 #-}
  {-# INLINE lazyUtf8 #-}
  words = T.words
  lines = T.lines
  unwords = T.unwords
  unlines = T.unlines
  text = id
  lazyText = lazy
  string = _Text
  utf8 = iso TE.encodeUtf8 TE.decodeUtf8
  lazyUtf8 = text . lazyUtf8

instance Textual LT.Text where
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE text #-}
  {-# INLINE lazyText #-}
  {-# INLINE string #-}
  {-# INLINE utf8 #-}
  {-# INLINE lazyUtf8 #-}
  words = LT.words
  lines = LT.lines
  unwords = LT.unwords
  unlines = LT.unlines
  text = strict
  lazyText = id
  string = _LText
  utf8 = text . utf8
  lazyUtf8 = iso LTE.encodeUtf8 LTE.decodeUtf8

instance a ~ Char => Textual [a] where
  {-# INLINE chars #-}
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE text #-}
  {-# INLINE lazyText #-}
  {-# INLINE string #-}
  {-# INLINE utf8 #-}
  {-# INLINE lazyUtf8 #-}
  chars = traversed
  words = List.words
  lines = List.lines
  unwords = List.unwords
  unlines = List.unlines
  text = from _Text
  lazyText = from _LText
  string = id
  utf8 = iso UTF8.fromString UTF8.toString
  lazyUtf8 = iso LUTF8.fromString LUTF8.toString

-- | utf8 encoded.
instance Textual BS.ByteString where
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE text #-}
  {-# INLINE lazyText #-}
  {-# INLINE string #-}
  {-# INLINE utf8 #-}
  {-# INLINE lazyUtf8 #-}
  words = BS8.words
  lines = UTF8.lines
  unwords = BS8.unwords
  unlines = BS8.unlines
  text = from utf8
  lazyText = from utf8
  string = from utf8
  utf8 = id
  lazyUtf8 = lazy

-- | utf8 encoded.
instance Textual LBS.ByteString where
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE text #-}
  {-# INLINE lazyText #-}
  {-# INLINE string #-}
  {-# INLINE utf8 #-}
  {-# INLINE lazyUtf8 #-}
  words = LBS8.words
  lines = LUTF8.lines
  unwords = LBS8.unwords
  unlines = LBS8.unlines
  text = from lazyUtf8
  lazyText = from lazyUtf8
  string = from lazyUtf8
  utf8 = strict
  lazyUtf8 = id


class Monoid b => Building b t | b -> t, t -> b where
  builder :: Iso' b t

  buildChar :: Char -> b

instance a ~ Char => Building (Endo [a]) [a] where
  builder = iso (`appEndo` []) (Endo #. (++))
  {-# INLINE builder #-}

  buildChar = Endo #. (:)

instance Building LTB.Builder LT.Text where
  builder = iso LTB.toLazyText LTB.fromLazyText
  {-# INLINE builder #-}

  buildChar = LTB.singleton

instance Building LBS.Builder LBS.ByteString where
  builder = iso LBS.toLazyByteString LBS.lazyByteString
  {-# INLINE builder #-}

  buildChar = LBS.charUtf8

-- | 'builder' onto the strict version.
builder' :: (Building b l, Strict l s) => Iso' b s
builder' = builder . strict

-- class Textual t where
--   text :: IndexedTraversal Int t Char

