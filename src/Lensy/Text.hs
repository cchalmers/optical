{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lensy.Text where

import Lensy.Containers

import Control.Lens

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.List as List

-- Chunked -------------------------------------------------------------

-- | Lazy constructs that are made of strict chunks.
class Strict l s => Chunked l s where
  chunked :: Iso' l [s]

instance Chunked LT.Text T.Text where
  chunked = iso LT.toChunks LT.fromChunks

instance Chunked LBS.ByteString BS.ByteString where
  chunked = iso LBS.toChunks LBS.fromChunks

-- Text ----------------------------------------------------------------

class Textual t where
  chars :: IndexedTraversal' Int t Char

  lined :: IndexedTraversal' Int t t

  worded :: IndexedTraversal' Int t t

  -- | Invalid encodeing is converted to empty chars (this should really
  --   be a prism but common librarys don't provide a way to easily do
  --   this)
  -- uft8Decoded :: Iso' t ByteString

  -- uft8Encoded :: Iso' ByteString t

  -- uft8Builder :: Iso' t Builder

instance Textual T.Text where
  chars = _Text . chars
  lined f = fmap T.unlines . traversed f . T.lines
  worded f = fmap T.unwords . traversed f . T.words

instance Textual LT.Text where
  chars = _LText . chars
  lined f = fmap LT.unlines . traversed f . LT.lines
  worded f = fmap LT.unwords . traversed f . LT.words

instance Textual String where
  chars = traversed
  lined f = fmap List.unlines . traversed f . List.lines
  worded f = fmap List.unwords . traversed f . List.words

-- utf8 :: Iso' Text ByteString
-- utf8 = iso encodeUtf8 decodeUtf8

-- instance Building (Endo [a]) [a] where
--   building =

-- class Textual t where
--   text :: IndexedTraversal Int t Char

