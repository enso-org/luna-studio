module NodeEditor.Batch.Connector.Commands where

import Common.Prelude

import qualified Data.Map                                 as Map
import qualified Data.Text                                as Text
import qualified LunaStudio.API.Atom.OpenFile             as OpenFile
import qualified LunaStudio.API.Graph.AddConnection       as AddConnection
import qualified LunaStudio.API.Graph.AddImports          as AddImports
import qualified LunaStudio.API.Graph.AddNode             as AddNode
import qualified LunaStudio.API.Graph.AddPort             as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph         as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes     as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollaborationUpdate as CollaborationUpdate
import qualified LunaStudio.API.Graph.CollapseToFunction  as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy                as Copy
import qualified LunaStudio.API.Graph.DumpGraphViz        as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram          as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs        as GetSubgraphs
import qualified LunaStudio.API.Graph.MovePort            as MovePort
import qualified LunaStudio.API.Graph.Paste               as Paste
import qualified LunaStudio.API.Graph.Redo                as Redo
import qualified LunaStudio.API.Graph.RemoveConnection    as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes         as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort          as RemovePort
import qualified LunaStudio.API.Graph.RenameNode          as RenameNode
import qualified LunaStudio.API.Graph.RenamePort          as RenamePort
import qualified LunaStudio.API.Graph.SaveSettings        as SaveSettings
import qualified LunaStudio.API.Graph.SearchNodes         as SearchNodes
import qualified LunaStudio.API.Graph.SetNodeExpression   as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta        as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault      as SetPortDefault
import qualified LunaStudio.API.Graph.Undo                as Undo
import qualified LunaStudio.API.Library.CreateLibrary     as CreateLibrary
import qualified LunaStudio.API.Library.ListLibraries     as ListLibraries
import qualified LunaStudio.Data.GraphLocation            as GraphLocation
import qualified LunaStudio.Data.NodeLoc                  as NodeLoc
import qualified LunaStudio.Data.Searcher.Hint.Library    as Library

import Common.Batch.Connector.Connection        (Message (Message), sendRequest,
                                                 sendUpdate)
import Data.Map                                 (Map)
import Data.Set                                 (Set)
import Data.UUID.Types                          (UUID)
import LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import LunaStudio.Data.Connection               (Connection)
import LunaStudio.Data.GraphLocation            (GraphLocation)
import LunaStudio.Data.Node                     (ExpressionNode)
import LunaStudio.Data.NodeLoc                  (NodeLoc, normalise, normalise',
                                                 normalise_)
import LunaStudio.Data.NodeMeta                 (NodeMeta)
import LunaStudio.Data.PortDefault              (PortDefault)
import LunaStudio.Data.PortRef                  (AnyPortRef (InPortRef'),
                                                 InPortRef, OutPortRef)
import LunaStudio.Data.Position                 (Position)
import LunaStudio.Data.Project                  (LocationSettings, ProjectId)
import NodeEditor.Batch.Workspace               (Workspace)
import NodeEditor.Batch.Workspace               (currentLocation)
import NodeEditor.React.Model.Connection        (ConnectionId)



withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f $ w ^. currentLocation


createLibrary :: Text -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
createLibrary name path _workspace uuid guiID = sendRequest
    $ Message uuid guiID $ CreateLibrary.Request
        $notImplemented
        (Just $ Text.unpack name)
        (Text.unpack path)

listLibraries :: ProjectId -> UUID -> Maybe UUID -> IO ()
listLibraries pid uuid guiID = sendRequest $ Message uuid guiID
    $ ListLibraries.Request pid

openFile :: FilePath -> UUID -> Maybe UUID -> IO ()
openFile path uuid guiID = sendRequest $ Message uuid guiID
    $ OpenFile.Request path

dumpGraphViz :: Workspace -> UUID -> Maybe UUID -> IO ()
dumpGraphViz workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace DumpGraphViz.Request


getProgram :: Maybe (GraphLocation, LocationSettings) -> Bool -> Workspace
    -> UUID -> Maybe UUID -> IO ()
getProgram maySettings retrieveLocation workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ (withLibrary workspace GetProgram.Request) maySettings retrieveLocation

addConnection :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc
    -> Workspace -> UUID -> Maybe UUID -> IO ()
addConnection src dst workspace uuid guiID = sendRequest
    $ Message uuid guiID $ addConnectionRequest src dst workspace

addConnectionRequest :: Either OutPortRef NodeLoc -> Either AnyPortRef NodeLoc
    -> Workspace -> AddConnection.Request
addConnectionRequest src dst workspace = withLibrary workspace $ \location ->
    AddConnection.Request location (conv src) dst where
        conv (Left a)  = Left a --TODO normalise
        conv (Right a) = Right $ a ^. NodeLoc.nodeId

addImports :: Set Library.Name -> Workspace -> UUID -> Maybe UUID -> IO ()
addImports libs workspace uuid guiID = sendRequest . Message uuid guiID
    . withLibrary workspace AddImports.Request $ libs

addNode :: NodeLoc -> Text -> NodeMeta -> Maybe NodeLoc -> Workspace -> UUID
    -> Maybe UUID -> IO ()
addNode nodeLoc expression meta connectTo workspace uuid guiID =
    sendRequest $ Message uuid guiID $ (withLibrary workspace' AddNode.Request)
        nodeLoc' expression meta (convert <$> connectTo) where
            (workspace', nodeLoc') = normalise workspace nodeLoc

addPort :: OutPortRef -> Maybe InPortRef -> Maybe Text -> Workspace -> UUID
    -> Maybe UUID -> IO ()
addPort portRef connDst name workspace uuid guiID = sendRequest
    $ Message uuid guiID $ (withLibrary workspace' AddPort.Request)
        portRef' (InPortRef' <$> maybeToList connDst) name where
            (workspace', portRef') = normalise workspace portRef

addSubgraph :: [ExpressionNode] -> [Connection] -> Workspace -> UUID
    -> Maybe UUID -> IO ()
addSubgraph nodes connections workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ (withLibrary workspace AddSubgraph.Request) nodes connections

autolayoutNodes :: [NodeLoc] -> Bool ->  Workspace -> UUID -> Maybe UUID -> IO ()
autolayoutNodes nls shouldCenter workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ (withLibrary workspace AutolayoutNodes.Request) nls shouldCenter

collapseToFunction :: [NodeLoc] -> Workspace -> UUID -> Maybe UUID ->  IO ()
collapseToFunction nodeLocs workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ withLibrary workspace' CollapseToFunction.Request nodeLocs' where
            (workspace', nodeLocs') = normalise' workspace nodeLocs

copy :: [NodeLoc] -> Workspace -> UUID -> Maybe UUID ->  IO ()
copy nodeLocs workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace' Copy.Request nodeLocs' where
        (workspace', nodeLocs') = normalise' workspace nodeLocs

getSubgraph :: NodeLoc -> Workspace -> UUID -> Maybe UUID -> IO ()
getSubgraph nodeLoc workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace $ GetSubgraphs.Request
        . (GraphLocation.breadcrumb .~ NodeLoc.toBreadcrumb
            (NodeLoc.prependPath workspace nodeLoc))

movePort :: OutPortRef -> Int -> Workspace -> UUID -> Maybe UUID -> IO ()
movePort portRef newPortPos workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ (withLibrary workspace' MovePort.Request) portRef' newPortPos where
            (workspace', portRef') = normalise workspace portRef

paste :: Position -> String -> Workspace -> UUID -> Maybe UUID ->  IO ()
paste cursorPos clipboardData workspace uuid guiID = sendRequest
    $ Message uuid guiID
        $ (withLibrary workspace Paste.Request) cursorPos clipboardData

redo :: UUID -> Maybe UUID -> IO ()
redo uuid guiID = sendRequest $ Message uuid guiID
    $ Redo.Request Redo.RedoRequest

removeConnection :: ConnectionId -> Workspace -> UUID -> Maybe UUID -> IO ()
removeConnection connId workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace' RemoveConnection.Request connId' where
        (workspace', connId') = normalise workspace connId

removeNodes :: [NodeLoc] -> Workspace -> UUID -> Maybe UUID ->  IO ()
removeNodes nodeLocs workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace' RemoveNodes.Request nodeLocs' where
        (workspace', nodeLocs') = normalise' workspace nodeLocs

removePort :: OutPortRef -> Workspace -> UUID -> Maybe UUID -> IO ()
removePort portRef workspace uuid guiID = sendRequest $ Message uuid guiID
    $ (withLibrary workspace' RemovePort.Request) portRef' where
        (workspace', portRef') = normalise workspace portRef

renameNode :: NodeLoc -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
renameNode nl name workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace' RenameNode.Request nodeId name where
        (workspace', nodeId) = normalise_ workspace nl

renamePort :: OutPortRef -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
renamePort portRef name workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace' RenamePort.Request portRef' name where
        (workspace', portRef') = normalise workspace portRef

saveSettings :: LocationSettings -> Workspace -> UUID -> Maybe UUID -> IO ()
saveSettings settings workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace SaveSettings.Request settings

searchNodes :: Set Library.Name -> Workspace -> UUID -> Maybe UUID -> IO ()
searchNodes libsNames workspace uuid guiID = sendRequest $ Message uuid guiID
    $ withLibrary workspace SearchNodes.Request libsNames

setNodeExpression :: NodeLoc -> Text -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodeExpression nodeLoc expression workspace uuid guiID = sendRequest
    $ Message uuid guiID $ withLibrary workspace'
        SetNodeExpression.Request nodeId expression where
            (workspace', nodeId) = normalise_ workspace nodeLoc

setNodesMeta :: Map NodeLoc NodeMeta -> Workspace -> UUID -> Maybe UUID -> IO ()
setNodesMeta updates workspace uuid guiID = sendRequest $ Message uuid guiID
    $ setNodesMetaRequest updates workspace 

setNodesMetaRequest :: Map NodeLoc NodeMeta -> Workspace -> SetNodesMeta.Request
setNodesMetaRequest updates workspace = withLibrary workspace $ \location ->
    SetNodesMeta.Request location (Map.mapKeys convert updates') where
        nls      = snd $ normalise' workspace $ Map.keys updates
        updates' = Map.fromList $ zip nls $ Map.elems updates

sendNodesMetaUpdate :: Map NodeLoc NodeMeta -> Workspace -> UUID -> Maybe UUID
    -> IO ()
sendNodesMetaUpdate updates workspace _ _ = sendUpdate $ withLibrary workspace'
    SetNodesMeta.Update (Map.mapKeys convert updates') where
        (workspace', nls) = normalise' workspace $ Map.keys updates
        updates'          = Map.fromList $ zip nls $ Map.elems updates

setPortDefault :: InPortRef -> PortDefault -> Workspace -> UUID -> Maybe UUID
    -> IO ()
setPortDefault portRef portDef workspace uuid guiID = sendRequest
    $ Message uuid guiID $ withLibrary workspace'
        SetPortDefault.Request portRef' (Just portDef) where
            (workspace', portRef') = normalise workspace portRef

undo :: UUID -> Maybe UUID -> IO ()
undo uuid guiID = sendRequest $ Message uuid guiID
    $ Undo.Request Undo.UndoRequest



requestCollaborationRefresh :: ClientId -> Workspace -> IO ()
requestCollaborationRefresh clientId workspace = sendUpdate
    $ withLibrary workspace
        CollaborationUpdate.Update clientId CollaborationUpdate.Refresh

collaborativeTouch :: ClientId -> [NodeLoc] -> Workspace -> IO ()
collaborativeTouch clientId locs workspace = sendUpdate
    $ withLibrary workspace
        CollaborationUpdate.Update clientId $ CollaborationUpdate.Touch locs

collaborativeModify :: ClientId -> [NodeLoc] -> Workspace -> IO ()
collaborativeModify clientId locs workspace = sendUpdate
    $ withLibrary workspace
        CollaborationUpdate.Update clientId  $ CollaborationUpdate.Modify locs

cancelCollaborativeTouch :: ClientId -> [NodeLoc] -> Workspace -> IO ()
cancelCollaborativeTouch clientId locs workspace = sendUpdate
    $ withLibrary workspace
        CollaborationUpdate.Update clientId $ CollaborationUpdate.CancelTouch locs
