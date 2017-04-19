module Node.Editor.Action.Basic.MovePort where

import           Empire.API.Data.PortRef                  (OutPortRef (OutPortRef), srcPortId)
import           Node.Editor.Action.Basic.AddConnection   (localAddConnection)
import           Node.Editor.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified Node.Editor.Action.Batch                 as Batch
import           Node.Editor.Action.Command               (Command)
import           Node.Editor.Action.State.NodeEditor      (getConnectionsContainingNode, getInputNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Connection       (dst, src)
import           Node.Editor.React.Model.Node.SidebarNode (countProjectionPorts, hasPort, inputSidebarPorts, isInputSidebar)
import           Node.Editor.React.Model.Port             (OutPortIndex (Projection))
import           Node.Editor.State.Global                 (State)


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
