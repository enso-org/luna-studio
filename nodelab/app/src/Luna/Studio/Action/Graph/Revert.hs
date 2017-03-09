module Luna.Studio.Action.Graph.Revert where

import           Empire.API.Data.GraphLocation             (GraphLocation)
import qualified Empire.API.Data.Node                      as Node
import qualified Empire.API.Graph.AddNode                  as AddNode
import qualified Empire.API.Graph.AddPort                  as AddPort
import qualified Empire.API.Graph.AddSubgraph              as AddSubgraph
import qualified Empire.API.Graph.Code                     as Code
import qualified Empire.API.Graph.Connect                  as Connect
import qualified Empire.API.Graph.GetProgram               as GetProgram
import qualified Empire.API.Graph.MonadsUpdate             as MonadsUpdate
import qualified Empire.API.Graph.NodeResultUpdate         as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch               as NodeSearch
import qualified Empire.API.Graph.NodesUpdate              as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate    as NodeTCUpdate
import qualified Empire.API.Graph.RemoveConnection         as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes              as RemoveNodes
import qualified Empire.API.Graph.RemovePort               as RemovePort
import qualified Empire.API.Graph.RenameNode               as RenameNode
import qualified Empire.API.Graph.RenamePort               as RenamePort
import           Empire.API.Graph.Request                  (GraphRequest)
import           Empire.API.Graph.Request                  (GraphRequest)
import qualified Empire.API.Graph.SetCode                  as SetCode
import qualified Empire.API.Graph.UpdateNodeMeta           as UpdateNodeMeta
import           Empire.API.Response                       (ResponseResult, Status)
import qualified Empire.API.Response                       as Response
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.Graph.Connect          (localConnect)
import           Luna.Studio.Action.Graph.RemoveConnection (localRemoveConnection)
import           Luna.Studio.Action.Node.Remove            (localRemoveNodes)
import qualified Luna.Studio.Batch.Workspace               as Workspace
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                  (State)
import qualified Luna.Studio.State.Global                  as Global
-- import Luna.Studio.Action.Graph.RemovePort (localRemovePort)


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl


revertAddNode :: AddNode.Request -> Command State ()
revertAddNode (AddNode.Request loc nodeId _ _ _) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ localRemoveNodes [nodeId]

revertAddPort :: AddPort.Request -> Command State ()
revertAddPort (AddPort.Request loc portRef) =
    whenM (isCurrentLocationAndGraphLoaded loc) $notImplemented -- $ localRemovePort portRef

revertAddSubgraph :: AddSubgraph.Request -> Command State ()
revertAddSubgraph (AddSubgraph.Request loc nodes _) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ localRemoveNodes $ flip map nodes $ view Node.nodeId

revertConnect :: Connect.Request -> Command State ()
revertConnect (Connect.Request location _ dst) =
    either (void . localRemoveConnection) (const $ return ()) dst

revertRemoveConnection :: RemoveConnection.Request -> Response.Status RemoveConnection.Inverse -> Command State ()
revertRemoveConnection (RemoveConnection.Request location dst) (Response.Ok (RemoveConnection.Inverse src)) =
    localConnect src dst
revertRemoveConnection (RemoveConnection.Request _location _dst) (Response.Error _msg) = $notImplemented
