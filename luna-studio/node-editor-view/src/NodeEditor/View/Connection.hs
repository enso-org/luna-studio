module NodeEditor.View.Connection where

import           Common.Data.JSON                  (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                as Lens
import           Data.Aeson                        (ToJSON (toEncoding, toJSON))
import           Data.Convert                      (Convertible (convert))
import qualified Data.HashMap.Strict               as HashMap
import qualified LunaStudio.Data.PortRef           as PortRef
import           NodeEditor.React.Model.Connection (Connection, ConnectionsMap)
import qualified NodeEditor.React.Model.Connection as Connection
import           NodeEditor.View.Diff              (DiffT, diffApply)


data ConnectionView = ConnectionView
    { _key :: String
    , _srcNode :: String
    , _srcPort :: String
    , _dstNode :: String
    , _dstPort :: String
    } deriving (Generic, Show)

makeLenses ''ConnectionView

instance ToJSON ConnectionView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible Connection ConnectionView where
    convert c = ConnectionView
        {- key        -} (c ^. Connection.connectionId . to show)
        {- srcNode    -} (c ^. Connection.src . PortRef.srcNodeLoc . to show)
        {- srcPort    -} (c ^. Connection.src . PortRef.srcPortId  . to show)
        {- dstNode    -} (c ^. Connection.dst . PortRef.dstNodeLoc . to show)
        {- dstPort    -} (c ^. Connection.dst . PortRef.dstNodeId  . to show)

foreign import javascript safe "atomCallback.getNodeEditorView().setConnections($1)"
    setConnections__ :: JSVal -> IO ()

setConnections :: MonadIO m => [ConnectionView] -> m ()
setConnections = liftIO . setConnections__ <=< toJSONVal

connectionsView :: MonadIO m => DiffT ConnectionsMap m ()
connectionsView = diffApply $ setConnections . map convert . HashMap.elems
