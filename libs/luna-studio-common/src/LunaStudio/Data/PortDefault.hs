module LunaStudio.Data.PortDefault where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)



data PortValue = IntValue    Int
               | DoubleValue Double
               | BoolValue   Bool
               | StringValue String
               deriving (Eq, Generic, Show)

data PortDefault = Expression String
                 | Constant   PortValue
                 deriving (Eq, Generic, Show)

makePrisms ''PortValue
makePrisms ''PortDefault
instance Binary PortValue
instance NFData PortValue
instance Binary PortDefault
instance NFData PortDefault

stringify :: PortValue -> Text
stringify = pack . show
