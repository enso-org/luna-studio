{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StrictData        #-}
module NodeEditor.React.Model.Visualization
    ( module NodeEditor.React.Model.Visualization
    , module X
    ) where

import           Common.Prelude
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           LunaStudio.Data.Visualization as X (VisualizationId, VisualizationValue (..))
import           LunaStudio.Data.Visualizer    as X (Visualizer (Visualizer), VisualizerId (VisualizerId), VisualizerMatcher,
                                                     VisualizerName, VisualizerPath, VisualizerType (..), errorVisId, getMdVisualizer,
                                                     placeholderVisId, visualizerId, visualizerName, visualizerRelPath, visualizerType,
                                                     _InternalVisualizer, _LunaVisualizer, _ProjectVisualizer)


data VisualizationMode = Default
                       | Focused
                       | Preview
                       | FullScreen
                       deriving (Eq, Generic, Show)

instance Default VisualizationMode where def = Default

data VisualizationStatus = Ready
                         | Outdated
                         deriving (Eq, Generic, Show)

data VisualizationParent = Node NodeLoc
                         | Searcher
                         deriving (Eq, Generic, Show)

data VisualizerProperties = VisualizerProperties { _runningVisualizer    :: Visualizer
                                                 , _selectedVisualizerId :: Maybe VisualizerId
                                                 } deriving (Eq, Generic, Show)

data RunningVisualization = RunningVisualization { _visualizationId      :: VisualizationId
                                                 , _visualizationMode    :: VisualizationMode
                                                 , _visualizerProperties :: VisualizerProperties
                                                 } deriving (Eq, Generic, Show)

data IdleVisualization = IdleVisualization { _visualizationStatus      :: VisualizationStatus
                                           , _idleVisualizerProperties :: VisualizerProperties
                                           } deriving (Eq, Generic, Show)

data NodeVisualizations = NodeVisualizations { _visualizations     :: Map VisualizationId RunningVisualization
                                             , _idleVisualizations :: [IdleVisualization]
                                             , _visualizers        :: Map VisualizerId VisualizerPath
                                             } deriving (Eq, Generic, Show)


data VisualizationProperties = VisualizationProperties { _visPropNodeLoc        :: NodeLoc
                                                       , _visPropIsNodeExpanded :: Bool
                                                       , _visPropArgPortsNumber :: Int
                                                       , _visPropVisualizers    :: Map VisualizerId VisualizerPath
                                                       , _visPropVisualization  :: RunningVisualization
                                                       } deriving (Eq, Generic, Show)

makePrisms ''VisualizationStatus
makePrisms ''VisualizationMode
makePrisms ''VisualizationParent
makeLenses ''VisualizerProperties
makeLenses ''RunningVisualization
makeLenses ''IdleVisualization
makeLenses ''NodeVisualizations
makeLenses ''VisualizationProperties


instance Default NodeVisualizations where def = NodeVisualizations def def def
instance NFData VisualizationMode
instance NFData VisualizationStatus
instance NFData VisualizationParent
instance NFData VisualizerProperties
instance NFData RunningVisualization
instance NFData IdleVisualization
instance NFData NodeVisualizations
instance NFData VisualizationProperties



toIdleVisualization :: VisualizationStatus -> RunningVisualization -> IdleVisualization
toIdleVisualization vs = IdleVisualization vs . view visualizerProperties

stopVisualizations :: NodeVisualizations -> NodeVisualizations
stopVisualizations nodeVis = nodeVis & visualizations     .~ def
                                     & idleVisualizations .~ (nodeVis ^. idleVisualizations) <> (map (toIdleVisualization Ready) . Map.elems $ nodeVis ^. visualizations)


awaitingDataMsg, noVisMsg, noDataMsg :: Text
awaitingDataMsg = "AWAITING_DATA"
noVisMsg        = "NO_VIS_FOR_TYPE"
noDataMsg       = "NO_DATA"
