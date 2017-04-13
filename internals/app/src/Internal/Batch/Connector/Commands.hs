module Internal.Batch.Connector.Commands where

import qualified Data.Text                              as Text
import           Data.UUID.Types                        (UUID)
import qualified Empire.API.Atom.IsSaved              as IsSaved
import qualified Empire.API.Atom.CloseFile              as CloseFile
import qualified Empire.API.Atom.GetBuffer              as GetBuffer
import qualified Empire.API.Atom.OpenFile               as OpenFile
import qualified Empire.API.Atom.SaveFile               as SaveFile
import qualified Empire.API.Atom.SetProject             as SetProject
import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Data.Breadcrumb             as Breadcrumb
import           Empire.API.Data.Connection             (Connection, ConnectionId)
import           Empire.API.Data.GraphLocation          (GraphLocation)
import qualified Empire.API.Data.GraphLocation          as GraphLocation
import           Empire.API.Data.Node                   (Node, NodeId)
import           Empire.API.Data.NodeMeta               (NodeMeta)
import           Empire.API.Data.PortDefault            (PortDefault)
import           Empire.API.Data.PortRef                (AnyPortRef, InPortRef, OutPortRef)
import           Empire.API.Data.Project                (ProjectId)
import           Internal.Batch.Connector.Connection (Message (Message), sendRequest, sendUpdate)
import           Internal.Batch.Workspace            (Workspace)
import           Internal.Batch.Workspace            (currentLocation)
import           Internal.Prelude


import Data.Text (unpack, pack)


closeFile :: FilePath -> UUID -> Maybe UUID -> IO ()
closeFile path uuid guiID = sendRequest $ Message uuid guiID $ CloseFile.Request path

getBuffer :: FilePath -> Maybe (Int, Int) -> UUID -> Maybe UUID -> IO ()
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
