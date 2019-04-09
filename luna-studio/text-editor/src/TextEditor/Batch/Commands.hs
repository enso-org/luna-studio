module TextEditor.Batch.Commands where

import Common.Prelude

import qualified LunaStudio.API.Atom.CloseFile      as CloseFile
import qualified LunaStudio.API.Atom.Copy           as Copy
import qualified LunaStudio.API.Atom.CreateProject  as CreateProject
import qualified LunaStudio.API.Atom.FileChanged    as FileChanged
import qualified LunaStudio.API.Atom.GetBuffer      as GetBuffer
import qualified LunaStudio.API.Atom.IsSaved        as IsSaved
import qualified LunaStudio.API.Atom.MoveProject    as MoveProject
import qualified LunaStudio.API.Atom.OpenFile       as OpenFile
import qualified LunaStudio.API.Atom.Paste          as Paste
import qualified LunaStudio.API.Atom.SaveFile       as SaveFile
import qualified LunaStudio.API.Atom.SetProject     as SetProject
import qualified LunaStudio.API.Atom.Substitute     as Substitute
import qualified LunaStudio.API.Control.Interpreter as Interpreter
import qualified LunaStudio.API.Graph.Redo          as Redo
import qualified LunaStudio.API.Graph.Undo          as Undo

import Common.Batch.Connector.Connection (Message (Message), sendRequest)
import Data.UUID.Types                   (UUID)
import JS.Atom                           (activeLocation)
import LunaStudio.Data.GraphLocation     (GraphLocation (..))
import LunaStudio.Data.Range             (Range)
import LunaStudio.Data.TextDiff          (TextDiff)

closeFile :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
closeFile path uuid guiID = sendRequest . Message uuid guiID
    $ CloseFile.Request path

createProject :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
createProject path uuid guiID = sendRequest . Message uuid guiID
    $ CreateProject.Request path

fileChanged :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
fileChanged path uuid guiID = sendRequest . Message uuid guiID
    $ FileChanged.Request path

getBuffer :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
getBuffer path uuid guiID = sendRequest . Message uuid guiID
    $ GetBuffer.Request path

isSaved :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
isSaved path uuid guiID = sendRequest . Message uuid guiID
    $ IsSaved.Request path

openFile :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
openFile path uuid guiID = sendRequest . Message uuid guiID
    $ OpenFile.Request path

saveFile :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
saveFile path uuid guiID = sendRequest . Message uuid guiID
    $ SaveFile.Request path

setProject :: MonadIO m => FilePath -> UUID -> Maybe UUID -> m ()
setProject rootPath uuid guiID = sendRequest . Message uuid guiID
    $ SetProject.Request rootPath

moveProject :: MonadIO m => FilePath -> FilePath -> UUID -> Maybe UUID -> m ()
moveProject oldPath newPath uuid guiID = sendRequest . Message uuid guiID
    $ MoveProject.Request oldPath newPath

substitute
    :: MonadIO m => GraphLocation -> [TextDiff] -> UUID -> Maybe UUID -> m ()
substitute location diffs uuid guiID =
    sendRequest . Message uuid guiID $ Substitute.Request location diffs

copy :: MonadIO m => FilePath -> [Range] -> UUID -> Maybe UUID -> m ()
copy path spans uuid guiID = sendRequest . Message uuid guiID
    $ Copy.Request path spans

paste :: MonadIO m
      => GraphLocation -> [Range] -> [Text] -> UUID -> Maybe UUID -> m ()
paste location spans content uuid guiID = sendRequest . Message uuid guiID
    $ Paste.Request location spans content

undo :: MonadIO m => UUID -> Maybe UUID -> m ()
undo uuid guiID
    = sendRequest . Message uuid guiID $ Undo.Request Undo.UndoRequest

redo :: MonadIO m => UUID -> Maybe UUID -> m ()
redo uuid guiID
    = sendRequest . Message uuid guiID $ Redo.Request Redo.RedoRequest

interpreterPause :: MonadIO m => UUID -> Maybe UUID -> m ()
interpreterPause uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest . Message uuid guiID $ Interpreter.Request loc Interpreter.Pause

interpreterStart :: MonadIO m => UUID -> Maybe UUID -> m ()
interpreterStart uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest . Message uuid guiID $ Interpreter.Request loc Interpreter.Start

interpreterReload :: MonadIO m => UUID -> Maybe UUID -> m ()
interpreterReload uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest . Message uuid guiID $ Interpreter.Request loc Interpreter.Reload
