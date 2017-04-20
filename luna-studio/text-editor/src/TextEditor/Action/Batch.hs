module TextEditor.Action.Batch  where

import           Data.UUID.Types                      (UUID)
import           TextEditor.Action.Command           (Command)
import           TextEditor.Action.UUID              (registerRequest)
import qualified TextEditor.Batch.Commands as BatchCmd
import           Common.Prelude
import           TextEditor.State.Global             (State, clientId)



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
