module LunaStudio.Data.Visualization where

import           Control.Lens       (makePrisms)
import           Data.Aeson.Types   (FromJSON, ToJSON)
import           Data.Binary        (Binary)
import           Data.Portable.Text (PortableText)
import           Data.UUID.Types    (UUID)
import           Prologue           hiding (Text)


type VisualizationId   = UUID
data VisualizationValue
    = Value PortableText
    | StreamStart
    | StreamDataPoint PortableText
    deriving (Eq, Generic, Show)

makePrisms ''VisualizationValue

instance Binary   VisualizationValue
instance NFData   VisualizationValue
instance FromJSON VisualizationValue
instance ToJSON   VisualizationValue
