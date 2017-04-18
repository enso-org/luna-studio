module Luna.Studio.Action.Basic.RemovePort where

import           Empire.API.Data.PortRef                   (OutPortRef (OutPortRef), srcPortId)
import           Luna.Studio.Action.Basic.AddConnection    (localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (removeConnection)
import           Luna.Studio.Action.Basic.UpdateNode       (localUpdateInputNode)
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.State.NodeEditor       (getConnectionsContainingNode, getInputNode)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection        (connectionId, dst, src)
import           Luna.Studio.React.Model.Node.SidebarNode  (countProjectionPorts, hasPort, inputSidebarPorts, isInputSidebar)
import           Luna.Studio.React.Model.Port              (OutPortIndex (Projection))
import           Luna.Studio.State.Global                  (State)


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
