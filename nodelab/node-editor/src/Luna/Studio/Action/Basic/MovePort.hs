module Luna.Studio.Action.Basic.MovePort where

import           Empire.API.Data.PortRef                  (OutPortRef (OutPortRef), srcPortId)
import           Luna.Studio.Action.Basic.AddConnection   (localAddConnection)
import           Luna.Studio.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified Luna.Studio.Action.Batch                 as Batch
import           Luna.Studio.Action.Command               (Command)
import           Luna.Studio.Action.State.NodeEditor      (getConnectionsContainingNode, getInputNode)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection       (dst, src)
import           Luna.Studio.React.Model.Node.SidebarNode (countProjectionPorts, hasPort, inputSidebarPorts, isInputSidebar)
import           Luna.Studio.React.Model.Port             (OutPortIndex (Projection))
import           Luna.Studio.State.Global                 (State)


movePort :: OutPortRef -> Int -> Command State ()
movePort portRef newPos = withJustM (localMovePort portRef newPos) $ const $ Batch.movePort portRef newPos

localMovePort :: OutPortRef -> Int -> Command State (Maybe OutPortRef)
localMovePort (OutPortRef nid pid@(Projection pos : p')) newPos = do
    if pos == newPos then return Nothing else do
        mayNode <- getInputNode nid
        flip (maybe (return Nothing)) mayNode $ \node -> do
            if     not (isInputSidebar node)
                || not (hasPort pid node)
                || newPos >= countProjectionPorts node
                || newPos < 0 then return Nothing
            else do
                let oldPorts    = node ^. inputSidebarPorts
                    lower       = min pos newPos
                    upper       = max pos newPos
                    (a, node1:b) = splitAt lower oldPorts
                    (c, node2:d) = splitAt (upper - lower - 1) b
                    newPorts = a <> [node2] <> c <> [node1] <> d
                void . localUpdateInputNode $ node & inputSidebarPorts .~ newPorts
                conns <- getConnectionsContainingNode nid
                forM_ conns $ \conn -> case conn ^. src of
                    OutPortRef srcNid (Projection i : p) ->
                        when (srcNid == nid) $
                            if i == pos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection newPos : p) (conn ^. dst)
                            else if i > pos && i <= newPos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i-1) : p) (conn ^. dst)
                            else if i < pos && i >= newPos
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i+1) : p) (conn ^. dst)
                                else return ()
                    _ -> return ()
                return . Just $ OutPortRef nid (Projection newPos : p')
localMovePort _ _ = $notImplemented
