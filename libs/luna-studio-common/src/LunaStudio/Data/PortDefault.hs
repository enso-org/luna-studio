module LunaStudio.Data.PortDefault where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)


data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Eq, Generic, NFData, Show)

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
makePrisms ''VisualizationValue
instance Binary PortValue
instance Binary PortDefault
instance Binary VisualizationValue

stringify :: PortValue -> Text
stringify = pack . show
