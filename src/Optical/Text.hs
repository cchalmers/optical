{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Optical.Text where

import           Optical.Containers

import           Control.Lens

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

import Prelude hiding (words, unwords, lines, unlines)

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

instance Textual T.Text where
  {-# INLINE chars #-}
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  chars = _Text . chars
  words = T.words
  lines = T.lines
  unwords = T.unwords
  unlines = T.unlines

instance Textual LT.Text where
  {-# INLINE chars #-}
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  chars = _LText . chars
  words = LT.words
  lines = LT.lines
  unwords = LT.unwords
  unlines = LT.unlines

instance Textual String where
  {-# INLINE chars #-}
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  chars = traversed
  words = List.words
  lines = List.lines
  unwords = List.unwords
  unlines = List.unlines

-- utf8String :: Iso' ByteString String

-- utf8 :: Iso' Text ByteString
-- utf8 = iso encodeUtf8 decodeUtf8

-- instance Building (Endo [a]) [a] where
--   building =

-- class Textual t where
--   text :: IndexedTraversal Int t Char

