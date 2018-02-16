module LunaStudio.Data.Visualization where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Data.Text        (Text)
import           Data.UUID.Types  (UUID)
import           Prologue         hiding (Text)

type VisualizationData  = [Text]
type VisualizationId    = UUID
data VisualizationValue = Value Text
                        | StreamStart
                        | StreamDataPoint Text
                        deriving (Eq, Generic, Show)

makePrisms ''VisualizationValue
instance Binary   VisualizationValue
instance NFData   VisualizationValue
instance FromJSON VisualizationValue
instance ToJSON   VisualizationValue
