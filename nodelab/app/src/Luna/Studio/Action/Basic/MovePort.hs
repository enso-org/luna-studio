module Luna.Studio.Action.Basic.MovePort where

import           Empire.API.Data.PortRef                (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), srcPortId, toAnyPortRef)
import           Luna.Studio.Action.Basic.AddConnection (localAddConnection)
import           Luna.Studio.Action.Basic.UpdateNode    (localUpdateEdgeNode)
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (getConnectionsContainingNode, getEdgeNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection     (dst, src)
import           Luna.Studio.React.Model.Node.EdgeNode  (countProjectionPorts, getPorts, hasPort, isInputEdge, ports)
import           Luna.Studio.React.Model.Port           (OutPort (Projection), PortId (OutPortId), portId, toPortsMap)
import           Luna.Studio.State.Global               (State)


movePort :: AnyPortRef -> Int -> Command State ()
movePort portRef newPos = withJustM (localMovePort portRef newPos) $ const $ Batch.movePort portRef newPos

localMovePort :: AnyPortRef -> Int -> Command State (Maybe AnyPortRef)
localMovePort (OutPortRef' (OutPortRef nid pid@(Projection pos))) newPos = do
    if pos == newPos then return Nothing else do
        mayNode <- getEdgeNode nid
        flip (maybe (return Nothing)) mayNode $ \node -> do
            if     (not . isInputEdge $ node)
                || (not $ hasPort (OutPortId pid) node)
                || newPos >= countProjectionPorts node
                || newPos < 0 then return Nothing
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
                void . localUpdateEdgeNode $ node & ports .~ newPortsMap
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
                return . Just . toAnyPortRef nid $ OutPortId (Projection newPos)
localMovePort _ _ = $notImplemented
