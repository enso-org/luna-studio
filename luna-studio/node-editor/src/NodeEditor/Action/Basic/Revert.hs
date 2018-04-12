module NodeEditor.Action.Basic.Revert where

import           Common.Action.Command                     (Command)
import           Common.Prelude
import qualified Data.Map                                  as Map
import qualified LunaStudio.API.Graph.AddConnection        as AddConnection
import qualified LunaStudio.API.Graph.AddNode              as AddNode
import qualified LunaStudio.API.Graph.AddPort              as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph          as AddSubgraph
import qualified LunaStudio.API.Graph.MovePort             as MovePort
import qualified LunaStudio.API.Graph.RemoveConnection     as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes          as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort           as RemovePort
import qualified LunaStudio.API.Graph.RenameNode           as RenameNode
import qualified LunaStudio.API.Graph.RenamePort           as RenamePort
import qualified LunaStudio.API.Graph.SetNodeExpression    as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta         as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault       as SetPortDefault
import qualified LunaStudio.API.Response                   as Response
import           LunaStudio.Data.Connection                (Connection (Connection), dst, src)
import qualified LunaStudio.Data.Connection                as Connection
import           LunaStudio.Data.Node                      (nodeId)
import           LunaStudio.Data.NodeLoc                   (prependPath)
import           LunaStudio.Data.PortRef                   (AnyPortRef (InPortRef'), OutPortRef (OutPortRef))
import           NodeEditor.Action.Basic.AddConnection     (localAddConnection, localAddConnections)
import           NodeEditor.Action.Basic.AddPort           (localAddPort)
import           NodeEditor.Action.Basic.AddSubgraph       (localAddSubgraph)
import           NodeEditor.Action.Basic.MovePort          (localMovePort)
import           NodeEditor.Action.Basic.RemoveConnection  (localRemoveConnection)
import           NodeEditor.Action.Basic.RemoveNode        (localRemoveNode, localRemoveNodes)
import           NodeEditor.Action.Basic.RemovePort        (localRemovePort)
import           NodeEditor.Action.Basic.RenameNode        (localRenameNode)
import           NodeEditor.Action.Basic.RenamePort        (localRenamePort)
import           NodeEditor.Action.Basic.SetNodeExpression (localSetNodeExpression)
import           NodeEditor.Action.Basic.SetNodeMeta       (localSetNodesMeta)
import           NodeEditor.Action.Basic.SetPortDefault    (localSetPortDefault)
import qualified NodeEditor.Action.Batch                   as Batch
import           NodeEditor.Action.State.Graph             (inCurrentLocation)
import           NodeEditor.React.Model.Port               (OutPortIndex (Projection))
import           NodeEditor.State.Global                   (State)


panic :: Command State ()
panic = Batch.getProgram def False

revertAddConnection :: AddConnection.Request -> Command State ()
revertAddConnection (AddConnection.Request loc _ (Left (InPortRef' dst'))) =
    inCurrentLocation loc
        $ \path -> void $ localRemoveConnection $ prependPath path dst'
revertAddConnection (AddConnection.Request _ _ (Left _)) = return ()
revertAddConnection _ = panic


revertAddNode :: AddNode.Request -> Command State ()
revertAddNode (AddNode.Request loc nl _ _ _) = inCurrentLocation loc
    $ \path -> void $ localRemoveNode $ prependPath path nl

revertAddPort :: AddPort.Request -> Command State ()
revertAddPort (AddPort.Request loc portRef _ _) = inCurrentLocation loc
    $ \path -> void $ localRemovePort $ prependPath path portRef

revertAddSubgraph :: AddSubgraph.Request -> Command State ()
revertAddSubgraph (AddSubgraph.Request loc nodes _) = inCurrentLocation loc $
    \path -> void . localRemoveNodes
        $ (convert . (path,) . view nodeId) <$> nodes

revertMovePort :: MovePort.Request -> Command State ()
revertMovePort (MovePort.Request loc oldPortRef newPos) =
    inCurrentLocation loc $ \path -> case oldPortRef of
        OutPortRef nid (Projection i : p) ->
            void $ localMovePort
                (prependPath path (OutPortRef nid $ Projection newPos : p)) i
        _                                           -> panic

revertRemoveConnection :: RemoveConnection.Request
    -> Response.Status RemoveConnection.Inverse -> Command State ()
revertRemoveConnection
    (RemoveConnection.Request loc dst')
    (Response.Ok (RemoveConnection.Inverse src')) = inCurrentLocation loc
        $ \path -> void . localAddConnection
            $ Connection (prependPath path src') (prependPath path dst')
revertRemoveConnection
    (RemoveConnection.Request _loc _dst)
    (Response.Error _msg) = panic

--TODO[LJK]: Force LunaStudio.Data.Connection to be instance of wrapped to make functions like this cleaner
revertRemoveNodes :: RemoveNodes.Request -> Response.Status RemoveNodes.Inverse
    -> Command State ()
revertRemoveNodes
    (RemoveNodes.Request loc _)
    (Response.Ok (RemoveNodes.Inverse nodes conns)) = inCurrentLocation loc
        $ \path -> do
            let nodes' = map (convert . (path,)) nodes
            void . localAddSubgraph nodes'
                $ Connection.prependPath path <$> conns
revertRemoveNodes (RemoveNodes.Request _loc _) (Response.Error _msg) = panic

revertRemovePort :: RemovePort.Request -> Response.Status RemovePort.Inverse
    -> Command State ()
revertRemovePort
    (RemovePort.Request loc portRef)
    (Response.Ok (RemovePort.Inverse prevName conns)) = inCurrentLocation loc
    $ \path -> do
        void $ localAddPort (prependPath path portRef) Nothing (Just prevName)
        void . localAddConnections $ Connection.prependPath path <$> conns
revertRemovePort (RemovePort.Request _loc _portRef) (Response.Error _msg)
    = panic

revertRenameNode :: RenameNode.Request -> Response.Status RenameNode.Inverse
    -> Command State ()
revertRenameNode
    (RenameNode.Request loc nid _)
    (Response.Ok (RenameNode.Inverse prevName)) = inCurrentLocation loc
        $ \path -> void $ localRenameNode (convert (path, nid)) $ Just prevName
revertRenameNode (RenameNode.Request _loc _nid _) (Response.Error _msg) = panic

revertRenamePort :: RenamePort.Request -> Response.Status RenamePort.Inverse
    -> Command State ()
revertRenamePort
    (RenamePort.Request loc portRef _)
    (Response.Ok (RenamePort.Inverse prevName)) = inCurrentLocation loc
        $ \path -> void $ localRenamePort (prependPath path portRef) prevName
revertRenamePort (RenamePort.Request _loc _portRef _) (Response.Error _msg)
    = panic

revertSetNodeExpression :: SetNodeExpression.Request
    -> Response.Status SetNodeExpression.Inverse -> Command State ()
revertSetNodeExpression
    (SetNodeExpression.Request loc nid _)
    (Response.Ok (SetNodeExpression.Inverse prevCode)) = inCurrentLocation loc
        $ \path -> void $ localSetNodeExpression (convert (path, nid)) prevCode
revertSetNodeExpression
    (SetNodeExpression.Request _loc _nid _)
    (Response.Error _msg) = panic

revertSetNodesMeta :: SetNodesMeta.Request
    -> Response.Status SetNodesMeta.Inverse -> Command State ()
revertSetNodesMeta
    (SetNodesMeta.Request loc _)
    (Response.Ok (SetNodesMeta.Inverse prevMeta)) = inCurrentLocation loc
        $ \path -> void . localSetNodesMeta
            $ Map.mapKeys (convert . (path,)) prevMeta
revertSetNodesMeta (SetNodesMeta.Request _loc _) (Response.Error _msg) = panic

revertSetPortDefault :: SetPortDefault.Request
    -> Response.Status SetPortDefault.Inverse -> Command State ()
revertSetPortDefault
    (SetPortDefault.Request loc portRef _)
    (Response.Ok (SetPortDefault.Inverse prevCode)) = inCurrentLocation loc
        $ \path ->
            mapM_(localSetPortDefault (prependPath path portRef)) prevCode
revertSetPortDefault
    (SetPortDefault.Request _loc _portRef _)
    (Response.Error _msg) = panic
