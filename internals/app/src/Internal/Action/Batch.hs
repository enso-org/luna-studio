module Internal.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           Empire.API.Data.Connection           (Connection, ConnectionId)
import           Empire.API.Data.Node                 (Node, NodeId)
import           Empire.API.Data.NodeMeta             (NodeMeta)
import           Empire.API.Data.PortDefault          (PortDefault)
import           Empire.API.Data.PortRef              (AnyPortRef, InPortRef, OutPortRef, dstNodeId, nodeId)
import           Empire.API.Data.Project              (ProjectId)
import           Internal.Action.Command           (Command)
import           Internal.Action.UUID              (registerRequest)
import qualified Internal.Batch.Connector.Commands as BatchCmd
import           Internal.Batch.Workspace          (Workspace)
import           Internal.Prelude
import           Internal.State.Global             (State, clientId, workspace)


withWorkspace :: (Workspace -> UUID -> Maybe UUID -> IO ()) -> Command State ()
withWorkspace act = do
    uuid       <- registerRequest
    guiID      <- use clientId
    workspace' <- use workspace
    liftIO $ act workspace' uuid $ Just guiID

withWorkspace' :: (Workspace -> IO ()) -> Command State ()
withWorkspace' act = do
    workspace' <- use workspace
    liftIO $ act workspace'

withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use clientId
    liftIO $ act uuid $ Just guiID


closeFile :: FilePath -> Command State ()
closeFile = withUUID . BatchCmd.closeFile

getBuffer :: FilePath -> Maybe (Int, Int) -> Command State ()
getBuffer = withUUID .: BatchCmd.getBuffer

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile

saveFile :: FilePath -> Command State ()
saveFile = withUUID . BatchCmd.saveFile

setProject :: FilePath -> Command State ()
setProject = withUUID . BatchCmd.setProject

substitute :: FilePath -> Int -> Int -> Text -> Maybe Int -> Command State ()
substitute = withUUID .::. BatchCmd.substitute