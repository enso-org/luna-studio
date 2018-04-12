module NodeEditor.Action.Basic.AddPort where

import           Common.Action.Command                   (Command)
import           Common.Prelude
import           LunaStudio.Data.Connection              (Connection (Connection))
import           LunaStudio.Data.LabeledTree             (LabeledTree (LabeledTree))
import           LunaStudio.Data.Port                    (Port (Port))
import           LunaStudio.Data.PortRef                 (InPortRef, OutPortRef (OutPortRef), srcPortId)
import           LunaStudio.Data.TypeRep                 (TypeRep (TStar))
import           NodeEditor.Action.Basic.AddConnection   (localAddConnection)
import           NodeEditor.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified NodeEditor.Action.Batch                 as Batch
import           NodeEditor.Action.State.NodeEditor      (getConnectionsContainingNode, getInputNode)
import qualified NodeEditor.React.Model.Connection       as Connection
import           NodeEditor.React.Model.Node.SidebarNode (countProjectionPorts, inputSidebarPorts)
import           NodeEditor.React.Model.Port             (OutPortIndex (Projection), OutPorts (OutPorts), PortState (NotConnected))
import           NodeEditor.State.Global                 (State)


addPort :: OutPortRef -> Maybe InPortRef -> Maybe Text -> Command State ()
addPort portRef connDst mayName = whenM (localAddPort portRef connDst mayName)
    $ Batch.addPort portRef connDst mayName

localAddPort :: OutPortRef -> Maybe InPortRef -> Maybe Text -> Command State Bool
localAddPort portRef@(OutPortRef nid pid@[Projection pos]) mayConnDst mayName = do
    mayNode <- getInputNode nid
    flip (maybe (return False)) mayNode $ \node ->
        if pos > countProjectionPorts node || pos < 0
            then return False
            else do
                let newPort     = LabeledTree (OutPorts []) $ convert
                        $ Port pid (fromMaybe def mayName) TStar NotConnected
                    oldPorts    = node ^. inputSidebarPorts
                    (portsBefore, portsAfter) = splitAt pos oldPorts
                    newPorts    = portsBefore <> [newPort] <> portsAfter
                void . localUpdateInputNode
                    $ node & inputSidebarPorts .~ newPorts
                conns <- getConnectionsContainingNode nid
                forM_ conns $ \conn -> case conn ^. Connection.src of
                    (OutPortRef srcNid (Projection i : p)) -> do
                        let correctPort = srcNid == nid && i >= pos
                        when correctPort $ void . localAddConnection
                            $ Connection
                                (conn ^. Connection.src
                                    & srcPortId .~ (Projection (i+1):p))
                                (conn ^. Connection.dst)
                    _ -> return ()
                withJust mayConnDst
                    $ void . localAddConnection . Connection portRef
                return True
localAddPort _ _ _ = $notImplemented
