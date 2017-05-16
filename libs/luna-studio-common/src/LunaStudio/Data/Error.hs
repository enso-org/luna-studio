module LunaStudio.Data.Error where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Prologue


data ErrorType = CompileError | RuntimeError deriving (Eq, Generic, NFData, Show)

data Error = Error ErrorType Text deriving (Eq, Generic, NFData, Show)

instance Binary ErrorType
instance Binary Error
