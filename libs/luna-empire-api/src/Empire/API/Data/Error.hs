{-# LANGUAGE DeriveAnyClass #-}
module Empire.API.Data.Error where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Prologue

data Error t = ImportError String | NoMethodError String t | TypeError t t | RuntimeError String deriving (Show, Eq, Generic, NFData)

instance Binary t => Binary (Error t)
