module LunaStudio.Data.PortDefault where

import           Control.Lens     (makePrisms)
import           Control.DeepSeq  (NFData)
import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Data.Text        (Text, pack)
import           Prologue         hiding (Text)


data PortValue
    = IntValue  Integer
    | RealValue Double
    | BoolValue Bool
    | TextValue String
    deriving (Eq, Generic, Show)

data PortDefault
    = Expression String
    | Constant   PortValue
    deriving (Eq, Generic, Show)

makePrisms ''PortValue
makePrisms ''PortDefault

instance Binary PortValue
instance NFData PortValue
instance FromJSON PortValue
instance ToJSON PortValue
instance Binary PortDefault
instance NFData PortDefault
instance FromJSON PortDefault
instance ToJSON PortDefault

stringify :: PortValue -> Text
stringify = pack . show
