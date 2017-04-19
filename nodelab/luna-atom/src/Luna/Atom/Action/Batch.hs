module Luna.Atom.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           Luna.Atom.Action.Command           (Command)
import           Luna.Atom.Action.UUID              (registerRequest)
import qualified Luna.Atom.Batch.Connector.Commands as BatchCmd
import           Luna.Prelude
import           Luna.Atom.State.Global             (State, clientId)



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
