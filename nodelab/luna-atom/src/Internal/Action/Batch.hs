module Internal.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           Internal.Action.Command           (Command)
import           Internal.Action.UUID              (registerRequest)
import qualified Internal.Batch.Connector.Commands as BatchCmd
import           Internal.Batch.Workspace          (Workspace)
import           Luna.Prelude
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

isSaved :: FilePath -> Command State ()
isSaved = withUUID . BatchCmd.isSaved

setProject :: FilePath -> Command State ()
setProject = withUUID . BatchCmd.setProject

substitute :: FilePath -> Int -> Int -> Text -> Maybe Int -> Command State ()
substitute = withUUID .::. BatchCmd.substitute
