module NodeEditor.Action.Basic.RenamePort where

import           Common.Action.Command                   (Command)
import           Common.Prelude
import           LunaStudio.Data.LabeledTree             (value)
import           NodeEditor.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified NodeEditor.Action.Batch                 as Batch
import           NodeEditor.Action.State.NodeEditor      (getInputNode)
import           NodeEditor.React.Model.Node.SidebarNode (hasPort, inputSidebarPorts, isInputSidebar)
import           NodeEditor.React.Model.Port             (OutPortIndex (Projection), OutPortRef (OutPortRef), name)
import           NodeEditor.State.Global                 (State)


renamePort :: OutPortRef -> Text -> Command State ()
renamePort portRef update =
    whenM (localRenamePort portRef update) $ Batch.renamePort portRef update

localRenamePort :: OutPortRef -> Text -> Command State Bool
localRenamePort (OutPortRef nl pid@(Projection pos : _)) update = do
    mayNode <- getInputNode nl
    flip (maybe (return False)) mayNode $ \node -> do
        let canBeUpdated = isInputSidebar node && hasPort pid node
        when canBeUpdated $ void . localUpdateInputNode $ node & inputSidebarPorts . element pos . value . name .~ update
        return canBeUpdated
localRenamePort _ _ = $notImplemented
