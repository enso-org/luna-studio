module NodeEditor.View.SidebarNode where

import           Common.Data.JSON                        (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                      as Lens
import           Data.Aeson                              (ToJSON (toEncoding, toJSON))
import           Data.Convert                            (Convertible (convert))
import           NodeEditor.React.Model.Node.SidebarNode (InputNode, OutputNode)
import qualified NodeEditor.React.Model.Node.SidebarNode as SidebarNode
import           NodeEditor.View.Diff                    (DiffT, diffApply)
import           NodeEditor.View.Port                    (PortView)
import           NodeEditor.View.Key                     (Key)


data SidebarNodeView = SidebarNodeView
    { _key      :: Key
    , _inPorts  :: [PortView]
    , _outPorts :: [PortView]
    } deriving (Generic, Show)

makeLenses ''SidebarNodeView

instance ToJSON SidebarNodeView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible InputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to convert)
        {- inPorts  -} def
        {- outPorts -} (n ^. to SidebarNode.outPortsList . to convert)

instance Convertible OutputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to convert)
        {- inPorts  -} (n ^. to SidebarNode.inPortsList . to convert)
        {- outPorts -} def

foreign import javascript safe "atomCallback.getNodeEditorView().setInputNode($1)"
    setInputNode__ :: JSVal -> IO ()

foreign import javascript safe "atomCallback.getNodeEditorView().setOutputNode($1)"
    setOutputNode__ :: JSVal -> IO ()

setInputNode :: MonadIO m => Maybe SidebarNodeView -> m ()
setInputNode = liftIO . setInputNode__ <=< toJSONVal

setOutputNode :: MonadIO m => Maybe SidebarNodeView -> m ()
setOutputNode = liftIO . setOutputNode__ <=< toJSONVal

inputNodeView :: MonadIO m => DiffT (Maybe InputNode) m ()
inputNodeView = diffApply $ setInputNode . fmap convert

outputNodeView :: MonadIO m => DiffT (Maybe OutputNode) m ()
outputNodeView = diffApply $ setOutputNode . fmap convert
