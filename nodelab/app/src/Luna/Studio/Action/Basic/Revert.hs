module Luna.Studio.Action.Basic.Revert where

import           Control.Arrow                              ((&&&))
import           Empire.API.Data.Connection                 (dst, src)
import           Empire.API.Data.Node                       (nodeId)
import           Empire.API.Data.PortRef                    (AnyPortRef (InPortRef'))
import qualified Empire.API.Graph.AddConnection             as AddConnection
import qualified Empire.API.Graph.AddNode                   as AddNode
import qualified Empire.API.Graph.AddPort                   as AddPort
import qualified Empire.API.Graph.AddSubgraph               as AddSubgraph
import qualified Empire.API.Graph.MovePort                  as MovePort
import qualified Empire.API.Graph.RemoveConnection          as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes               as RemoveNodes
import qualified Empire.API.Graph.RemovePort                as RemovePort
import qualified Empire.API.Graph.RenameNode                as RenameNode
import qualified Empire.API.Graph.RenamePort                as RenamePort
import qualified Empire.API.Graph.SetNodeCode               as SetNodeCode
import qualified Empire.API.Graph.SetNodeExpression         as SetNodeExpression
import qualified Empire.API.Graph.SetNodesMeta              as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault            as SetPortDefault
import qualified Empire.API.Response                        as Response
import           Luna.Studio.Action.Basic.AddConnection     (localAddConnection, localAddConnections)
import           Luna.Studio.Action.Basic.AddPort           (localAddPort)
import           Luna.Studio.Action.Basic.AddSubgraph       (localAddSubgraph)
import           Luna.Studio.Action.Basic.MovePort          (localMovePort)
import           Luna.Studio.Action.Basic.RemoveConnection  (localRemoveConnection)
import           Luna.Studio.Action.Basic.RemoveNode        (localRemoveNode, localRemoveNodes)
import           Luna.Studio.Action.Basic.RemovePort        (localRemovePort)
import           Luna.Studio.Action.Basic.RenameNode        (localRenameNode)
import           Luna.Studio.Action.Basic.SetNodeCode       (localSetNodeCode)
import           Luna.Studio.Action.Basic.SetNodeExpression (localSetNodeExpression)
import           Luna.Studio.Action.Basic.SetNodeMeta       (localSetNodesMeta)
import           Luna.Studio.Action.Basic.SetPortDefault    (localSetPortDefault)
import           Luna.Studio.Action.Command                 (Command)
import           Luna.Studio.Action.State.Graph             (isCurrentLocationAndGraphLoaded)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node               (_Expression)
import           Luna.Studio.State.Global                   (State)


revertAddConnection :: AddConnection.Request -> Command State ()
revertAddConnection (AddConnection.Request loc _ (Left (InPortRef' dst'))) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localRemoveConnection dst'
revertAddConnection (AddConnection.Request _ _ (Left _)) = return ()
revertAddConnection _ = $notImplemented


revertAddNode :: AddNode.Request -> Command State ()
revertAddNode (AddNode.Request loc nid _ _ _) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localRemoveNode nid

revertAddPort :: AddPort.Request -> Command State ()
revertAddPort (AddPort.Request loc portRef) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localRemovePort portRef

revertAddSubgraph :: AddSubgraph.Request -> Command State ()
revertAddSubgraph (AddSubgraph.Request loc nodes _) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void . localRemoveNodes $ map (view nodeId) nodes

revertMovePort :: MovePort.Request -> Command State ()
revertMovePort (MovePort.Request loc oldPortRef newPortRef) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localMovePort newPortRef oldPortRef

revertRemoveConnection :: RemoveConnection.Request -> Response.Status RemoveConnection.Inverse -> Command State ()
revertRemoveConnection (RemoveConnection.Request loc dst') (Response.Ok (RemoveConnection.Inverse src')) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localAddConnection src' dst'
revertRemoveConnection (RemoveConnection.Request _loc _dst) (Response.Error _msg) = $notImplemented

--TODO[LJK]: Force Empire.API.Data.Connection to be instance of wrapped to make functions like this cleaner
revertRemoveNodes :: RemoveNodes.Request -> Response.Status RemoveNodes.Inverse -> Command State ()
revertRemoveNodes (RemoveNodes.Request loc _) (Response.Ok (RemoveNodes.Inverse nodes conns)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localAddSubgraph nodes' (map (\conn -> (conn ^. src, conn ^. dst)) conns)
    where
        nodes' = (map convert nodes) ^.. traverse . _Expression
        conns' = map (view src &&& view dst) conns
revertRemoveNodes (RemoveNodes.Request _loc _) (Response.Error _msg) = $notImplemented

revertRemovePort :: RemovePort.Request -> Response.Status RemovePort.Inverse -> Command State ()
revertRemovePort (RemovePort.Request loc portRef) (Response.Ok (RemovePort.Inverse conns)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localAddPort portRef >> localAddConnections (map (\conn -> (conn ^. src, conn ^. dst)) conns)
revertRemovePort (RemovePort.Request _loc _portRef) (Response.Error _msg) = $notImplemented

revertRenameNode :: RenameNode.Request -> Response.Status RenameNode.Inverse -> Command State ()
revertRenameNode (RenameNode.Request loc nid _) (Response.Ok (RenameNode.Inverse prevName)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localRenameNode nid prevName
revertRenameNode (RenameNode.Request _loc _nid _) (Response.Error _msg) = $notImplemented

revertRenamePort :: RenamePort.Request -> Response.Status RenamePort.Inverse -> Command State ()
revertRenamePort (RenamePort.Request loc portRef _) (Response.Ok (RenamePort.Inverse prevName)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ $notImplemented
revertRenamePort (RenamePort.Request _loc _portRef _) (Response.Error _msg) = $notImplemented

revertSetNodeCode :: SetNodeCode.Request -> Response.Status SetNodeCode.Inverse -> Command State ()
revertSetNodeCode (SetNodeCode.Request loc nid _) (Response.Ok (SetNodeCode.Inverse prevCode)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localSetNodeCode nid prevCode
revertSetNodeCode (SetNodeCode.Request _loc _nid _) (Response.Error _msg) = $notImplemented

revertSetNodeExpression :: SetNodeExpression.Request -> Response.Status SetNodeExpression.Inverse -> Command State ()
revertSetNodeExpression (SetNodeExpression.Request loc nid _) (Response.Ok (SetNodeExpression.Inverse prevCode)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localSetNodeExpression nid prevCode
revertSetNodeExpression (SetNodeExpression.Request _loc _nid _) (Response.Error _msg) = $notImplemented

revertSetNodesMeta :: SetNodesMeta.Request -> Response.Status SetNodesMeta.Inverse -> Command State ()
revertSetNodesMeta (SetNodesMeta.Request loc _) (Response.Ok (SetNodesMeta.Inverse prevMeta)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void . localSetNodesMeta $ map convert prevMeta
revertSetNodesMeta (SetNodesMeta.Request _loc _) (Response.Error _msg) = $notImplemented

revertSetPortDefault :: SetPortDefault.Request -> Response.Status SetPortDefault.Inverse -> Command State ()
revertSetPortDefault (SetPortDefault.Request loc nid _) (Response.Ok (SetPortDefault.Inverse prevCode)) =
    whenM (isCurrentLocationAndGraphLoaded loc) $ void $ localSetPortDefault nid prevCode
revertSetPortDefault (SetPortDefault.Request _loc _nid _) (Response.Error _msg) = $notImplemented
