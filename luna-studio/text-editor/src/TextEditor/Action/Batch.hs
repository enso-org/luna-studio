module TextEditor.Action.Batch  where

import           Common.Action.Command         (Command)
import           Common.Prelude
import           Data.UUID.Types               (UUID)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Point         (Point)
import           TextEditor.Action.UUID        (registerRequest)
import qualified TextEditor.Batch.Commands     as BatchCmd
import           TextEditor.State.Global       (State, clientId)


withUUID :: (UUID -> Maybe UUID -> IO ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use clientId
    liftIO $ act uuid $ Just guiID

closeFile :: FilePath -> Command State ()
closeFile = withUUID . BatchCmd.closeFile

getBuffer :: FilePath -> Maybe [(Int, Int)] -> Command State ()
getBuffer = withUUID .: BatchCmd.getBuffer

fileChanged :: FilePath -> Command State ()
fileChanged = withUUID . BatchCmd.fileChanged

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile

saveFile :: FilePath -> Command State ()
saveFile = withUUID . BatchCmd.saveFile

isSaved :: FilePath -> Command State ()
isSaved = withUUID . BatchCmd.isSaved

setProject :: FilePath -> Command State ()
setProject = withUUID . BatchCmd.setProject

substitute :: GraphLocation -> Point -> Point -> Text -> Maybe Point -> Command State ()
substitute = withUUID .::. BatchCmd.substitute
