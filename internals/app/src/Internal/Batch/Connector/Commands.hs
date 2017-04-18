module Internal.Batch.Connector.Commands where

import           Data.UUID.Types                        (UUID)
import qualified Empire.API.Atom.IsSaved              as IsSaved
import qualified Empire.API.Atom.CloseFile              as CloseFile
import qualified Empire.API.Atom.GetBuffer              as GetBuffer
import qualified Empire.API.Atom.OpenFile               as OpenFile
import qualified Empire.API.Atom.SaveFile               as SaveFile
import qualified Empire.API.Atom.SetProject             as SetProject
import qualified Empire.API.Atom.Substitute             as Substitute
import           Internal.Batch.Connector.Connection (Message (Message), sendRequest, sendUpdate)
import           Internal.Prelude


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
