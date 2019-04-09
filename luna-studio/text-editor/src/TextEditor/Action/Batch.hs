module TextEditor.Action.Batch  where

import Common.Prelude

import qualified TextEditor.Batch.Commands as BatchCmd
import qualified TextEditor.State.Global   as State

import Common.Action.Command         (Command)
import Control.Monad.State           (MonadState)
import Data.UUID.Types               (UUID)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.Range         (Range)
import LunaStudio.Data.TextDiff      (TextDiff)
import TextEditor.Action.UUID        (registerRequest)
import TextEditor.State.Global       (State, clientId)


withUUID :: (UUID -> Maybe UUID -> Command State ()) -> Command State ()
withUUID act = do
    uuid  <- registerRequest
    guiID <- use clientId
    act uuid $ Just guiID

closeFile :: FilePath -> Command State ()
closeFile = withUUID . BatchCmd.closeFile

createProject :: FilePath -> Command State ()
createProject = withUUID . BatchCmd.createProject

getBuffer :: FilePath -> Command State ()
getBuffer = withUUID . BatchCmd.getBuffer

fileChanged :: FilePath -> Command State ()
fileChanged = withUUID . BatchCmd.fileChanged

openFile :: FilePath -> Command State ()
openFile = withUUID . BatchCmd.openFile

saveFile :: FilePath -> Command State ()
saveFile = withUUID . BatchCmd.saveFile

isSaved :: FilePath -> Command State ()
isSaved = withUUID . BatchCmd.isSaved

moveProject :: FilePath -> FilePath -> Command State ()
moveProject = withUUID .: BatchCmd.moveProject

setProject :: FilePath -> Command State ()
setProject = withUUID . BatchCmd.setProject

substitute :: GraphLocation -> [TextDiff] -> Command State ()
substitute location diffs = withUUID $ \uuid guiId -> do
    State.ignoreResponse uuid
    BatchCmd.substitute location diffs uuid guiId

copy :: FilePath -> [Range] -> Command State ()
copy = withUUID .: BatchCmd.copy

paste :: GraphLocation -> [Range] -> [Text] -> Command State ()
paste = withUUID .:. BatchCmd.paste

interpreterStart :: Command State ()
interpreterStart = withUUID BatchCmd.interpreterStart

interpreterPause :: Command State ()
interpreterPause = withUUID BatchCmd.interpreterPause

interpreterReload :: Command State ()
interpreterReload = withUUID BatchCmd.interpreterReload

undo :: Command State ()
undo = withUUID BatchCmd.undo

redo :: Command State ()
redo = withUUID BatchCmd.redo
