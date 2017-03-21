module Luna.Studio.Action.Basic.MovePort where

import           Empire.API.Data.PortRef                (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), srcPortId)
import           Luna.Studio.Action.Basic.AddConnection (localAddConnection)
import           Luna.Studio.Action.Basic.UpdateNode    (localUpdateNode)
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (getConnectionsContainingNode, getNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection     (dst, src)
import           Luna.Studio.React.Model.Node           (countProjectionPorts, getPorts, hasPort, isInputEdge, ports)
import           Luna.Studio.React.Model.Port           (OutPort (Projection), PortId (OutPortId), portId, toPortsMap)
import           Luna.Studio.State.Global               (State)


movePort :: AnyPortRef -> AnyPortRef -> Command State ()
movePort portRef newPortRef = whenM (localMovePort portRef newPortRef) $ Batch.movePort portRef newPortRef

localMovePort :: AnyPortRef -> AnyPortRef -> Command State Bool
localMovePort (OutPortRef' (OutPortRef nid pid@(Projection pos))) (OutPortRef' (OutPortRef newNid newPid@(Projection newPos))) =
    if nid /= newNid || pid == newPid then return False else do
        mayNode      <- getNode nid
        flip (maybe (return False)) mayNode $ \node ->
            if     (not . isInputEdge $ node)
                || hasPort (OutPortId pid) node
                || newPos >= countProjectionPorts node
                || newPos < 0 then return False
            else do
                let oldPorts    = getPorts node
                    newPorts    = flip map oldPorts $ \port' -> case port' ^. portId of
                        OutPortId (Projection i) ->
                            if i == pos
                                then port' & portId .~ OutPortId (Projection newPos)
                            else if i > pos && i <= newPos
                                then port' & portId .~ OutPortId (Projection (i-1))
                            else if i < pos && i >= newPos
                                then port' & portId .~ OutPortId (Projection (i+1))
                                else port'
                        _                        -> port'
                    newPortsMap = toPortsMap newPorts
                void . localUpdateNode $ node & ports .~ newPortsMap
                conns <- getConnectionsContainingNode nid
                forM_ conns $ \conn -> case conn ^. src of
                    OutPortRef srcNid (Projection i) ->
                        when (srcNid == nid) $
                            if i == pos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection newPos) (conn ^. dst)
                            else if i > pos && i <= newPos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i-1)) (conn ^. dst)
                            else if i < pos && i >= newPos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i+1)) (conn ^. dst)
                                else return ()
                    _ -> return ()
                return True
localMovePort _ _ = $notImplemented
