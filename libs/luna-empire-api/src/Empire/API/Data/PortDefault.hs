module Empire.API.Data.PortDefault where

import           Control.DeepSeq (NFData)
import           Data.Binary     (Binary)
import           Data.Text       (Text, pack)
import           Prologue        hiding (Text)


data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Generic, Eq, NFData, Show)

data Value = IntValue    Int
           | DoubleValue Double
           | BoolValue   Bool
           | StringValue String
           deriving (Generic, Eq, NFData, Show)

data PortDefault = Expression String
                 | Constant   Value
                 deriving (Generic, Eq, NFData, Show)

instance Binary Value
instance Binary PortDefault
instance Binary VisualizationValue

makePrisms ''Value
makePrisms ''PortDefault
makePrisms ''VisualizationValue

stringify :: Value -> Text
stringify = pack . show
