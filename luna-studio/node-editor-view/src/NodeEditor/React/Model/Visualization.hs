{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
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
                                                     VisualizerName, VisualizerPath, VisualizerType (..), getErrorVisualizer,
                                                     getMdVisualizer, visualizerId, visualizerName, visualizerRelPath, visualizerType)


data VisualizationMode = Default
                       | Focused
                       | Preview
                       | FullScreen
                       deriving (Eq, Generic, NFData, Show)

instance Default VisualizationMode where def = Default

data VisualizationStatus = Ready
                         | Outdated
                         deriving (Eq, Generic, NFData, Show)

data VisualizationParent = Node NodeLoc
                         | Searcher
                         deriving (Eq, Generic, NFData, Show)


data RunningVisualization = RunningVisualization { _visualizationId   :: VisualizationId
                                                 , _visualizationMode :: VisualizationMode
                                                 , _runningVisualizer :: Visualizer
                                                 } deriving (Eq, Generic, NFData, Show)

data IdleVisualization = IdleVisualization { _visualizationStatus :: VisualizationStatus
                                           , _idleVisualizer      :: Visualizer
                                           } deriving (Eq, Generic, NFData, Show)

data NodeVisualizations = NodeVisualizations { _visualizations     :: Map VisualizationId RunningVisualization
                                             , _idleVisualizations :: [IdleVisualization]
                                             , _visualizers        :: Map VisualizerId VisualizerPath
                                             } deriving (Eq, Generic, NFData, Show)


data VisualizationProperties = VisualizationProperties { _visPropNodeLoc        :: NodeLoc
                                                       , _visPropIsNodeExpanded :: Bool
                                                       , _visPropArgPortsNumber :: Int
                                                       , _visPropVisualizers    :: Map VisualizerId VisualizerPath
                                                       , _visPropVisualization  :: RunningVisualization
                                                       } deriving (Eq, Generic, NFData, Show)

makePrisms ''VisualizationStatus
makePrisms ''VisualizationMode
makePrisms ''VisualizationParent
makeLenses ''RunningVisualization
makeLenses ''IdleVisualization
makeLenses ''NodeVisualizations
makeLenses ''VisualizationProperties

instance Default NodeVisualizations where def = NodeVisualizations def def def

toIdleVisualization :: VisualizationStatus -> RunningVisualization -> IdleVisualization
toIdleVisualization vs = IdleVisualization vs . view runningVisualizer

stopVisualizations :: NodeVisualizations -> NodeVisualizations
stopVisualizations nodeVis = nodeVis & visualizations     .~ def
                                     & idleVisualizations .~ (nodeVis ^. idleVisualizations) <> (map (toIdleVisualization Ready) . Map.elems $ nodeVis ^. visualizations)
