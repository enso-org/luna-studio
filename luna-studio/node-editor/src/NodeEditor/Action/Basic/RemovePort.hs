module NodeEditor.Action.Basic.RemovePort where

import           Common.Action.Command                    (Command)
import           Common.Prelude
import           LunaStudio.Data.Connection               (Connection (Connection))
import           LunaStudio.Data.PortRef                  (OutPortRef, OutPortRef (OutPortRef), nodeLoc, srcPortId)
import           NodeEditor.Action.Basic.AddConnection    (localAddConnection)
import           NodeEditor.Action.Basic.RemoveConnection (localRemoveConnection)
import           NodeEditor.Action.Basic.UpdateNode       (localUpdateInputNode)
import qualified NodeEditor.Action.Batch                  as Batch
import           NodeEditor.Action.State.NodeEditor       (getConnectionsContainingNode, getInputNode)
import           NodeEditor.Action.State.ResetNode        (resetSuccessors)
import           NodeEditor.React.Model.Connection        (connectionId, dst, src)
import           NodeEditor.React.Model.Node.SidebarNode  (countProjectionPorts, hasPort, inputIsDef, inputSidebarPorts, isInputSidebar)
import           NodeEditor.React.Model.Port              (OutPortIndex (Projection))
import           NodeEditor.State.Global                  (State)


removePort :: OutPortRef -> Command State ()
removePort portRef = whenM (localRemovePort portRef) $ do
    resetSuccessors $ portRef ^. nodeLoc
    Batch.removePort portRef

localRemovePort :: OutPortRef -> Command State Bool
localRemovePort (OutPortRef nid pid@(Projection pos : _)) = do
    mayNode <- getInputNode nid
    flip (maybe (return False)) mayNode $ \node ->
        if not (isInputSidebar node) || not (hasPort pid node) || (not (node ^. inputIsDef) && countProjectionPorts node <= 1)
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
                                then void . localRemoveConnection $ conn ^. connectionId
                            else when (i >= pos) $
                                void . localAddConnection $ Connection (conn ^. src & srcPortId .~ Projection (i-1) : p) (conn ^. dst)

                    _ -> return ()
                return True
localRemovePort _ = $notImplemented
