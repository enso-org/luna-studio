module TextEditor.Batch.Commands where

import           Common.Batch.Connector.Connection  (Message (Message), sendRequest)
import           Common.Prelude                     hiding (span)
import           Data.UUID.Types                    (UUID)
import           JS.Atom                            (activeLocation)
import qualified LunaStudio.API.Atom.CloseFile      as CloseFile
import qualified LunaStudio.API.Atom.Copy           as Copy
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
import           LunaStudio.Data.GraphLocation      (GraphLocation (..))
import           LunaStudio.Data.Range              (Range)
import           LunaStudio.Data.TextDiff           (TextDiff)

-- Atom requests --

closeFile :: FilePath -> UUID -> Maybe UUID -> IO ()
closeFile path uuid guiID = sendRequest $ Message uuid guiID
    $ CloseFile.Request path

fileChanged :: FilePath -> UUID -> Maybe UUID -> IO ()
fileChanged path uuid guiID = sendRequest $ Message uuid guiID
    $ FileChanged.Request path

getBuffer :: FilePath -> UUID -> Maybe UUID -> IO ()
getBuffer path uuid guiID = sendRequest $ Message uuid guiID
    $ GetBuffer.Request path

isSaved :: FilePath -> UUID -> Maybe UUID -> IO ()
isSaved path uuid guiID = sendRequest $ Message uuid guiID
    $ IsSaved.Request path

openFile :: FilePath -> UUID -> Maybe UUID -> IO ()
openFile path uuid guiID = sendRequest $ Message uuid guiID
    $ OpenFile.Request path

saveFile :: FilePath -> UUID -> Maybe UUID -> IO ()
saveFile path uuid guiID = sendRequest $ Message uuid guiID
    $ SaveFile.Request path

setProject :: FilePath -> UUID -> Maybe UUID -> IO ()
setProject rootPath uuid guiID = sendRequest $ Message uuid guiID
    $ SetProject.Request rootPath

moveProject :: FilePath -> FilePath -> UUID -> Maybe UUID -> IO ()
moveProject oldPath newPath uuid guiID = sendRequest $ Message uuid guiID
    $ MoveProject.Request oldPath newPath

substitute :: GraphLocation -> [TextDiff] -> UUID -> Maybe UUID -> IO ()
substitute location diffs uuid guiID =
    sendRequest $ Message uuid guiID $ Substitute.Request location diffs

copy :: FilePath -> [Range] -> UUID -> Maybe UUID -> IO ()
copy path spans uuid guiID = sendRequest $ Message uuid guiID
    $ Copy.Request path spans

paste :: GraphLocation -> [Range] -> [Text] -> UUID -> Maybe UUID -> IO ()
paste location spans content uuid guiID = sendRequest $ Message uuid guiID
    $ Paste.Request location spans content

interpreterPause :: UUID -> Maybe UUID -> IO ()
interpreterPause uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest $ Message uuid guiID $ Interpreter.Request loc Interpreter.Pause

interpreterStart :: UUID -> Maybe UUID -> IO ()
interpreterStart uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest $ Message uuid guiID $ Interpreter.Request loc Interpreter.Start

interpreterReload :: UUID -> Maybe UUID -> IO ()
interpreterReload uuid guiID = do
    loc <- fromMaybe (GraphLocation def def) <$> activeLocation
    sendRequest $ Message uuid guiID $ Interpreter.Request loc Interpreter.Reload
