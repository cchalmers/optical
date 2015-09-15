module Optical.System
  (
    module System.FilePath
  , module System.FilePath.Lens
  , module System.Directory
  , module System.Environment
  , module System.Process

  , mkdir
  , ls
  , isDir
  ) where

import System.FilePath
import System.FilePath.Lens
import System.Directory
import System.Environment
import System.Process
import Control.Monad.IO.Class


mkdir :: MonadIO m => FilePath -> m ()
mkdir = liftIO . createDirectoryIfMissing False

ls :: MonadIO m => FilePath -> m [FilePath]
ls dir = do
  dir' <- liftIO $ canonicalizePath dir
  ps <- liftIO $ drop 2 <$> getDirectoryContents dir'
  return $ map (dir' </>) ps

-- | Check if 'FilePath' points to a directory.
isDir :: MonadIO m => FilePath -> m Bool
isDir = liftIO . doesDirectoryExist

