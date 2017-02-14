module Luna.Studio.Action.Node.Expand
    ( expandSelectedNodes
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         ()
import           Luna.Studio.Action.ConnectionPen   ()
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Action.Port.Self       (showOrHideSelfPort)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (connectAction, penConnectAction)
import           Luna.Studio.State.Global           (State, checkAction)
import qualified Luna.Studio.State.Global           as Global


expandSelectedNodes :: Command State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allExpanded = all (view Node.isExpanded) sn
    updatedNodes <- forM sn $ \node -> do
        let newNode = node & Node.isExpanded .~ if allExpanded then False else True
        mayConnect    <- checkAction connectAction
        mayPenConnect <- checkAction penConnectAction
        showOrHideSelfPort mayConnect mayPenConnect newNode
    Global.modifyNodeEditor $ forM_ updatedNodes $ \node ->
        NodeEditor.nodes . at (node ^. Node.nodeId) ?= node
    updateConnectionsForNodes $ map (view Node.nodeId) updatedNodes
