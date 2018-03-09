module NodeEditor.View.SidebarNode where

import           Common.Data.JSON                        (toJSONVal)
import           Common.Prelude
import           Data.Aeson                              (ToJSON)
import           Data.Convert                            (Convertible (convert))
import           NodeEditor.React.Model.Node.SidebarNode (InputNode, OutputNode)
import qualified NodeEditor.React.Model.Node.SidebarNode as SidebarNode
import           NodeEditor.View.Diff                    (DiffT, diffApply)
import           NodeEditor.View.Port                    (PortView)


inputNodeView :: MonadIO m => DiffT (Maybe InputNode) m ()
inputNodeView = diffApply $ setInputNode . convert

outputNodeView :: MonadIO m => DiffT (Maybe OutputNode) m ()
outputNodeView = diffApply $ setOutputNode . convert


data SidebarNodeView = SidebarNodeView
    { key      :: String
    , inPorts  :: [PortView]
    , outPorts :: [PortView]
    } deriving (Generic, Show)

instance ToJSON SidebarNodeView
instance Convertible InputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to show)
        {- inPorts  -} def
        {- outPorts -} (n ^. to SidebarNode.outPortsList . to convert)

instance Convertible OutputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to show)
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
