module Data.Path (module Path) where

import Prologue

import qualified Data.Binary as Binary
import qualified Path

import Data.Binary (Binary)
import Path        (Abs, Dir, File, Path, Rel)

-- | This module contains an orphan instance of Binary for Path.
--   Should be used throughout this package. We won't run into
--   orphan instance related problems as long as we never use
--   a Path as a bare message, always wrapping it in another
--   datatype.

instance Binary (Path Abs Dir) where
    put = Binary.put . Path.toFilePath
    get = do
        filepath <- Binary.get
        case Path.parseAbsDir filepath of
            Nothing -> fail "Not an absolute directory"
            Just p  -> pure p


instance Binary (Path Rel File) where
    put = Binary.put . Path.toFilePath
    get = do
        filepath <- Binary.get
        case Path.parseRelFile filepath of
            Nothing -> fail "Not a relative filepath"
            Just p  -> pure p

