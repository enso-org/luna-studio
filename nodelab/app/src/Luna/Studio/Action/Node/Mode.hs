module Luna.Studio.Action.Node.Mode
    ( selectedToggleMode
    , selectedToggleUnfold
    ) where

import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         ()
import           Luna.Studio.Action.ConnectionPen   ()
import qualified Luna.Studio.Action.Graph           as Graph
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Action.Port.Self       (showOrHideSelfPort)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Mode, Node)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (connectAction, penConnectAction)
import           Luna.Studio.State.Global           (State, checkAction)
import qualified Luna.Studio.State.Global           as Global


selectedToggleMode :: Mode -> Command State ()
selectedToggleMode newMode = do
    nodes <- selectedNodes
    let allNewMode = all (view $ Node.isMode newMode) nodes
    toggleLocal allNewMode newMode nodes

selectedToggleUnfold :: Command State ()
selectedToggleUnfold = do
    nodes <- selectedNodes
    let allNewMode = all (view $ Node.isExpandedFunction) nodes
    if allNewMode then do
        mapM_ Graph.localUnmerge nodes
        toggleLocal allNewMode def nodes
    else
        mapM_ (Batch.getSubgraph . Node._nodeId) nodes

toggleLocal :: Bool -> Mode -> [Node] -> Command State ()
toggleLocal allNewMode newMode nodes = do
    updatedNodes <- forM nodes $ \node -> do
        let newNode = node & Node.mode .~ if allNewMode then def else newMode
        mayConnect    <- checkAction connectAction
        mayPenConnect <- checkAction penConnectAction
        showOrHideSelfPort mayConnect mayPenConnect newNode
    Global.modifyNodeEditor $ forM_ updatedNodes $ \node ->
        NodeEditor.nodes . at (node ^. Node.nodeId) ?= node
    updateConnectionsForNodes $ map (view Node.nodeId) updatedNodes
