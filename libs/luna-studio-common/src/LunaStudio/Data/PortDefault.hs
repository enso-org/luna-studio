module LunaStudio.Data.PortDefault where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)



data PortValue = IntValue    Int
               | DoubleValue Double
               | BoolValue   Bool
               | StringValue String
               deriving (Eq, Generic, NFData, Show)

data PortDefault = Expression String
                 | Constant   PortValue
                 deriving (Eq, Generic, NFData, Show)

makePrisms ''PortValue
makePrisms ''PortDefault
instance Binary PortValue
instance Binary PortDefault

stringify :: PortValue -> Text
stringify = pack . show
