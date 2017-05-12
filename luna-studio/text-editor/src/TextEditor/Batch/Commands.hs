module TextEditor.Batch.Commands where

import           Data.UUID.Types                        (UUID)
import qualified LunaStudio.API.Atom.IsSaved              as IsSaved
import qualified LunaStudio.API.Atom.CloseFile              as CloseFile
import qualified LunaStudio.API.Atom.GetBuffer              as GetBuffer
import qualified LunaStudio.API.Atom.OpenFile               as OpenFile
import qualified LunaStudio.API.Atom.SaveFile               as SaveFile
import qualified LunaStudio.API.Atom.SetProject             as SetProject
import qualified LunaStudio.API.Atom.Substitute             as Substitute
import           Common.Batch.Connector.Connection (Message (Message), sendRequest)
import           Common.Prelude


-- Atom requests --

closeFile :: FilePath -> UUID -> Maybe UUID -> IO ()
closeFile path uuid guiID = sendRequest $ Message uuid guiID $ CloseFile.Request path

getBuffer :: FilePath -> Maybe [(Int, Int)] -> UUID -> Maybe UUID -> IO ()
getBuffer path maybeSpan uuid guiID = sendRequest $ Message uuid guiID $ GetBuffer.Request path maybeSpan

openFile :: FilePath -> UUID -> Maybe UUID -> IO ()
openFile path uuid guiID = sendRequest $ Message uuid guiID $ OpenFile.Request path

saveFile :: FilePath -> UUID -> Maybe UUID -> IO ()
saveFile path uuid guiID = sendRequest $ Message uuid guiID $ SaveFile.Request path

isSaved :: FilePath -> UUID -> Maybe UUID -> IO ()
isSaved path uuid guiID = sendRequest $ Message uuid guiID $ IsSaved.Request path

setProject :: FilePath -> UUID -> Maybe UUID -> IO ()
setProject rootPath uuid guiID = sendRequest $ Message uuid guiID $ SetProject.Request rootPath

substitute :: FilePath -> Int -> Int -> Text -> Maybe Int -> UUID -> Maybe UUID -> IO ()
substitute path start end text cursor uuid guiID =  sendRequest $ Message uuid guiID $ Substitute.Request path start end text cursor
