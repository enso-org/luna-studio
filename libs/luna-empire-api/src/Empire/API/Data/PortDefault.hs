module Empire.API.Data.PortDefault where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)


data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Generic, Eq, NFData, Show)

data PortValue = IntValue    Int
               | DoubleValue Double
               | BoolValue   Bool
               | StringValue String
               deriving (Generic, Eq, NFData, Show)

data PortDefault = Expression String
                 | Constant   PortValue
                 deriving (Generic, Eq, NFData, Show)

instance Binary PortValue
instance Binary PortDefault
instance Binary VisualizationValue

makePrisms ''PortValue
makePrisms ''PortDefault
makePrisms ''VisualizationValue

stringify :: PortValue -> Text
stringify = pack . show
