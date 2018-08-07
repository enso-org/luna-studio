{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StrictData        #-}
module NodeEditor.React.Model.Visualization
    ( module NodeEditor.React.Model.Visualization
    , module X
    ) where

import           Common.Prelude
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.UUID.Types               (UUID)
import           IdentityString                (IdentityString)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           LunaStudio.Data.Visualization as X (VisualizationId)
import           LunaStudio.Data.Visualizer    as X (Visualizer (Visualizer), VisualizerId (VisualizerId), VisualizerMatcher,
                                                     VisualizerName, VisualizerPath, VisualizerType (..), errorVisId, getMdVisualizer,
                                                     placeholderVisId, visualizerId, visualizerName, visualizerRelPath,
                                                     visualizerType, _InternalVisualizer, _LunaVisualizer, _ProjectVisualizer)

data Data
    = Value  IdentityString
    | Stream [IdentityString]
    deriving (Eq, Generic)

instance Show Data where
    show (Value _)  = "JSData Value"
    show (Stream _) = "JSData Stream"

data Content
    = Data    Data
    | Message Text
    | Error   Text
    deriving (Eq, Generic, Show)

type IframeId = UUID

data Mode
    = Default
    | Focused
    | Preview
    | FullScreen
    | Hidden
    deriving (Eq, Generic, Show)

instance Default Mode where def = Default

data Parent
    = Node NodeLoc
    | Searcher
    deriving (Eq, Generic, Show)

data Visualization = Visualization
    { _visualizationId      :: VisualizationId
    , _iframeId             :: UUID
    , _mode                 :: Mode
    , _visualizer           :: Visualizer
    , _selectedVisualizerId :: Maybe VisualizerId
    } deriving (Eq, Generic, Show)

data NodeVisualizations = NodeVisualizations
    { _visualizations             :: Map VisualizationId Visualization
    , _visualizers                :: Map VisualizerId VisualizerPath
    , _content                    :: Content
    , _visualizationsEnabled      :: Bool
    , _errorVisualizationsEnabled :: Bool
    } deriving (Eq, Generic, Show)


data VisualizationProperties = VisualizationProperties
    { _visPropNodeLoc        :: NodeLoc
    , _visPropIsNodeExpanded :: Bool
    , _visPropArgPortsNumber :: Int
    , _visPropVisualizers    :: Map VisualizerId VisualizerPath
    , _visPropVisualization  :: Visualization
    } deriving (Eq, Generic, Show)

makePrisms ''Data
makePrisms ''Content
makePrisms ''Mode
makePrisms ''Parent
makeLenses ''Visualization
makeLenses ''NodeVisualizations
makeLenses ''VisualizationProperties

instance NFData Data
instance NFData Content
instance NFData Mode
instance NFData Parent
instance NFData Visualization
instance NFData NodeVisualizations
instance NFData VisualizationProperties

currentInternalVisualizer :: Getter NodeVisualizations (Maybe VisualizerId)
currentInternalVisualizer = to getter where
    getter nv = case nv ^. content of
        Message {} -> Just placeholderVisId
        Error   {} -> Just errorVisId
        _          -> Nothing

-- isActive :: visualizationsEnabled -> errorVisualizationEnabled -> ...
isActive :: Bool -> Bool -> Content -> Visualization -> Bool
isActive _ True (Error {}) v
  = not (has (mode . _Hidden) v) && isErrorVisualization v
isActive _ False (Error {}) _ = False
isActive False _ _ _          = False
isActive True  _ _ v
  = not $ has (mode . _Hidden) v || isErrorVisualization v

isErrorVisualization :: Visualization -> Bool
isErrorVisualization
    = (errorVisId ==) . view (visualizer . visualizerId)

isPlaceholderVisualization :: Visualization -> Bool
isPlaceholderVisualization
    = (placeholderVisId ==) . view (visualizer . visualizerId)

dataVisualizations
    :: Getter NodeVisualizations (Map VisualizationId Visualization)
dataVisualizations = to getter where
    getter = Map.filter (not . isErrorVisualization) . view visualizations

errorVisualizations
    :: Getter NodeVisualizations (Map VisualizationId Visualization)
errorVisualizations = to getter where
    getter = Map.filter isErrorVisualization . view visualizations

activeVisualizations
    :: Getter NodeVisualizations (Map VisualizationId Visualization)
activeVisualizations = to getter where
    getter nv = let
            errVisEnabled = nv ^. errorVisualizationsEnabled
            visEnabled    = nv ^. visualizationsEnabled
            c             = nv ^. content
        in Map.filter
            (isActive visEnabled errVisEnabled c)
            $ nv ^. visualizations

awaitingDataMsg, noVisMsg, noDataMsg :: Text
awaitingDataMsg = "AWAITING_DATA"
noVisMsg        = "NO_VIS_FOR_TYPE"
noDataMsg       = "NO_DATA"
