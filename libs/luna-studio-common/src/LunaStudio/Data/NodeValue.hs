module LunaStudio.Data.NodeValue where

import           Data.Binary             (Binary)
import           Data.Text               (Text)
import           LunaStudio.Data.Error   (Error)
import           LunaStudio.Data.TypeRep (TypeRep)
import           Prologue                hiding (Text, TypeRep)

type VisualizerName    = Text
type VisualizerPath    = Text
type VisualizerMatcher = TypeRep -> IO (Maybe VisualizerPath)

type ShortValue = Text

data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Eq, Generic, NFData, Show)

data NodeValue = NodeValue ShortValue [VisualizationValue]
               | NodeError Error
               deriving (Eq, Generic, NFData, Show)

data NodeVisualization = NodeVisualization { _visualizerName     :: VisualizerName
                                           , _visualizerPath     :: VisualizerPath
                                           , _visualizationData  :: VisualizationValue
                                           } deriving (Eq, Generic, NFData, Show)

makePrisms ''NodeValue
makePrisms ''NodeVisualization
makePrisms ''VisualizationValue
instance Binary NodeValue
instance Binary VisualizationValue
