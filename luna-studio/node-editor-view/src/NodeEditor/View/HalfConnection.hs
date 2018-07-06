module NodeEditor.View.HalfConnection where

import           Common.Data.JSON                  (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                as Lens
import           Data.Aeson                        (ToJSON (toEncoding, toJSON))
import           Data.Convert                      (Convertible (convert))
import qualified LunaStudio.Data.PortRef           as PortRef
import           NodeEditor.React.Model.Connection (HalfConnection)
import qualified NodeEditor.React.Model.Connection as HalfConnection
import           NodeEditor.View.Diff              (DiffT, diffApply, diffConvert)
import           NodeEditor.View.Key               (Key)
import           LunaStudio.Data.Port              (AnyPortId (InPortId', OutPortId'))


data HalfConnectionView = HalfConnectionView
    { _srcNode  :: Key
    , _srcPort  :: Key
    , _reversed :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''HalfConnectionView

instance ToJSON HalfConnectionView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible HalfConnection HalfConnectionView where
    convert c = HalfConnectionView
        {- srcNode  -} (c ^. HalfConnection.from . PortRef.nodeLoc . to convert)
        {- srcPort  -} (c ^. HalfConnection.from . PortRef.portId  . to convert)
        {- reversed -} (c ^. HalfConnection.from . to PortRef.isInPortRef)

foreign import javascript safe "atomCallback.getNodeEditorView().setHalfConnections($1)"
    setHalfConnections__ :: JSVal -> IO ()

setHalfConnections :: MonadIO m => [HalfConnectionView] -> m ()
setHalfConnections = liftIO . setHalfConnections__ <=< toJSONVal

halfConnectionsView :: MonadIO m => DiffT [HalfConnection] m ()
halfConnectionsView = diffConvert $ diffApply setHalfConnections
