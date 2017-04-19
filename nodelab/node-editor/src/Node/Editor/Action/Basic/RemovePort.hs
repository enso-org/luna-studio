module Node.Editor.Action.Basic.RemovePort where

import           Empire.API.Data.PortRef                   (OutPortRef (OutPortRef), srcPortId)
import           Node.Editor.Action.Basic.AddConnection    (localAddConnection)
import           Node.Editor.Action.Basic.RemoveConnection (removeConnection)
import           Node.Editor.Action.Basic.UpdateNode       (localUpdateInputNode)
import qualified Node.Editor.Action.Batch                  as Batch
import           Node.Editor.Action.Command                (Command)
import           Node.Editor.Action.State.NodeEditor       (getConnectionsContainingNode, getInputNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Connection        (connectionId, dst, src)
import           Node.Editor.React.Model.Node.SidebarNode  (countProjectionPorts, hasPort, inputSidebarPorts, isInputSidebar)
import           Node.Editor.React.Model.Port              (OutPortIndex (Projection))
import           Node.Editor.State.Global                  (State)


removePort :: OutPortRef -> Command State ()
removePort portRef = whenM (localRemovePort portRef) $ Batch.removePort portRef

localRemovePort :: OutPortRef -> Command State Bool
localRemovePort (OutPortRef nid pid@(Projection pos : _)) = do
    mayNode <- getInputNode nid
    flip (maybe (return False)) mayNode $ \node ->
        if not (isInputSidebar node) || not (hasPort pid node) || countProjectionPorts node == 1
            then return False
            else do
                let (prev, _:next) = splitAt pos $ node ^. inputSidebarPorts
                    newPorts = prev <> next
                void . localUpdateInputNode $ node & inputSidebarPorts .~ newPorts
                conns <- getConnectionsContainingNode nid
                -- TODO[LJK]: Do it at once so we don't update the same Connection twice accidentaly
                forM_ conns $ \conn -> case conn ^. src of
                    OutPortRef srcNid (Projection i : p) ->
                        when (srcNid == nid) $
                            if i == pos
                                then void . removeConnection $ conn ^. connectionId
                            else if (i >= pos)
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i-1) : p) (conn ^. dst)
                                else return ()
                    _ -> return ()
                return True
localRemovePort _ = $notImplemented
