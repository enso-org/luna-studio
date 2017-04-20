module Empire.API.Data.Error where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Prologue

data ErrorType = CompileError | RuntimeError deriving (Show, Eq, Generic, NFData)

data Error = Error ErrorType Text deriving (Show, Eq, Generic, NFData)

instance Binary ErrorType
instance Binary Error
