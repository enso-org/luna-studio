module NodeEditor.View.Connection where

import           Common.Data.JSON                  (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                as Lens
import           Data.Aeson                        (ToJSON (toEncoding, toJSON))
import           Data.Convert                      (Convertible (convert))
import qualified LunaStudio.Data.PortRef           as PortRef
import           NodeEditor.React.Model.Connection (Connection, ConnectionsMap)
import qualified NodeEditor.React.Model.Connection as Connection
import           NodeEditor.View.Diff              (DiffT, diffApply, diffConvert, diffHashMap)
import           NodeEditor.View.Key               (Key)


data ConnectionView = ConnectionView
    { _key :: Key
    , _srcNode :: Key
    , _srcPort :: Key
    , _dstNode :: Key
    , _dstPort :: Key
    } deriving (Eq, Generic, Show)

makeLenses ''ConnectionView

instance ToJSON ConnectionView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible Connection ConnectionView where
    convert c = ConnectionView
        {- key        -} (c ^. Connection.connectionId . to convert)
        {- srcNode    -} (c ^. Connection.src . PortRef.srcNodeLoc . to convert)
        {- srcPort    -} (c ^. Connection.src . PortRef.srcPortId  . to convert)
        {- dstNode    -} (c ^. Connection.dst . PortRef.dstNodeLoc . to convert)
        {- dstPort    -} (c ^. Connection.dst . PortRef.dstPortId  . to convert)

foreign import javascript safe "atomCallback.getNodeEditorView().setConnection($1)"
    setConnection__ :: JSVal -> IO ()

foreign import javascript safe "atomCallback.getNodeEditorView().unsetConnection($1)"
    unsetConnection__ :: JSVal -> IO ()

setConnection :: MonadIO m => ConnectionView -> m ()
setConnection = liftIO . setConnection__ <=< toJSONVal

unsetConnection :: MonadIO m => ConnectionView -> m ()
unsetConnection = liftIO . unsetConnection__ <=< toJSONVal

connectionView :: MonadIO m => DiffT ConnectionView m ()
connectionView = diffApply setConnection

connectionsView :: MonadIO m => DiffT ConnectionsMap m ()
connectionsView = diffHashMap
    (diffConvert connectionView)
    (setConnection . convert)
    (unsetConnection . convert)
