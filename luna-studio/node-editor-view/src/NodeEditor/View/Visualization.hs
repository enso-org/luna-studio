module NodeEditor.View.Visualization where

import           Common.Data.JSON                     (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                   as Lens
import           Data.Aeson                           (ToJSON (toEncoding, toJSON))
import           Data.Convert                         (Convertible (convert))
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           NodeEditor.React.Model.Visualization (Mode, Mode (Default, Focused, FullScreen, Hidden, Preview), NodeVisualizations,
                                                       Visualization, VisualizationId, Visualizer, VisualizerId,
                                                       VisualizerType (InternalVisualizer, LunaVisualizer, ProjectVisualizer))
import qualified NodeEditor.React.Model.Visualization as Vis
import           NodeEditor.View.Diff                 (DiffT, diffApply, diffConvert, diffMapWithKey)
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           NodeEditor.View.Key                  (Key)


type VisualizerName = String

data VisualizerIdView = VisualizerIdView
    { _visualizerName :: String
    , _visualizerType :: String
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerIdView

data VisualizerView = VisualizerView
    { _visualizerId   :: VisualizerIdView
    , _visualizerPath :: String
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizerView

data VisualizationView = VisualizationView
    { _key                  :: Key
    , _iframeId             :: String
    , _mode                 :: String
    , _currentVisualizer    :: VisualizerView
    , _selectedVisualizer   :: Maybe VisualizerIdView
    } deriving (Eq, Generic, Show)

makeLenses ''VisualizationView

data NodeVisualizationsView = NodeVisualizationsView
    { _nodeKey        :: Key
    , _visualizations :: [VisualizationView]
    , _visualizers    :: [VisualizerIdView]
    } deriving (Eq, Generic, Show)

instance ToJSON VisualizerIdView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance ToJSON VisualizerView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance ToJSON VisualizationView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance ToJSON NodeVisualizationsView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible Mode String where
    convert Focused    = "Focused"
    convert Preview    = "Preview"
    convert FullScreen = "FullScreen"
    convert _          = "Default"

instance Convertible VisualizerType String where
    convert InternalVisualizer = "InternalVisualizer"
    convert LunaVisualizer     = "LunaVisualizer"
    convert ProjectVisualizer  = "ProjectVisualizer"

instance Convertible VisualizerId VisualizerIdView where
    convert vid = VisualizerIdView
        {- visualizerName -} (convert $ vid ^. Vis.visualizerName)
        {- visualizerType -} (convert $ vid ^. Vis.visualizerType)

instance Convertible Visualizer VisualizerView where
    convert v = VisualizerView
        {- visualizerId   -} (convert $ v ^. Vis.visualizerId)
        {- visualizerPath -} (convert $ v ^. Vis.visualizerRelPath)

instance Convertible Visualization VisualizationView where
    convert v = VisualizationView
        {- key                -} (convert $ v ^. Vis.visualizationId)
        {- iframeId           -} (convert $ v ^. Vis.iframeId)
        {- mode               -} (convert $ v ^. Vis.mode)
        {- currentVisualizer  -} (convert $ v ^. Vis.visualizer)
        {- selectedVisualizer -} (convert $ v ^. Vis.selectedVisualizerId)

instance Convertible (NodeLoc, NodeVisualizations) NodeVisualizationsView where
    convert (nl, nv) = NodeVisualizationsView
        {- nodeKey        -} (convert nl)
        {- visualizations -} (convert
            <$> Map.elems (nv ^. Vis.activeVisualizations))
        {- visualizers    -} (convert <$> Map.keys (nv ^. Vis.visualizers))


foreign import javascript safe
    "atomCallback.getNodeEditorView().setVisualization($1)"
    setVisualization__ :: JSVal -> IO ()

foreign import javascript safe
    "atomCallback.getNodeEditorView().unsetVisualization($1)"
    unsetVisualization__ :: JSVal -> IO ()


visualizationView :: MonadIO m => DiffT NodeVisualizationsView m ()
visualizationView = diffApply setVisualization

setVisualization :: MonadIO m => NodeVisualizationsView -> m ()
setVisualization v = liftIO $ setVisualization__ =<< toJSONVal v

unsetVisualization :: MonadIO m => NodeVisualizationsView -> m ()
unsetVisualization v = liftIO $ unsetVisualization__ =<< toJSONVal v

nodeVisualizationsView :: MonadIO m
    => DiffT (Map NodeLoc NodeVisualizations) m ()
nodeVisualizationsView = diffMapWithKey
    (diffConvert visualizationView)
    (setVisualization . convert)
    (unsetVisualization . convert)
