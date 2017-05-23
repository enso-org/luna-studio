module LunaStudio.Data.NodeValue where

import           Data.Binary           (Binary)
import           Data.Text             (Text)
import           LunaStudio.Data.Error (Error)
import           Prologue              hiding (Text, TypeRep)

type ShortValue = Text

data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Eq, Generic, NFData, Show)

data NodeValue = NodeValue ShortValue [VisualizationValue]
               | NodeError Error
               deriving (Eq, Generic, NFData, Show)

data NodeVisualization = NodeVisualization { _visualizationValue :: VisualizationValue
                                           , _visualizationId    :: Int
                                           , _isPinned           :: Bool
                                           } deriving (Eq, Generic, NFData, Show)

makePrisms ''NodeValue
makePrisms ''NodeVisualization
makePrisms ''VisualizationValue
instance Binary NodeValue
instance Binary VisualizationValue
