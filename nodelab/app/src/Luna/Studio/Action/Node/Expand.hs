module Luna.Studio.Action.Node.Expand
    ( expandSelectedNodes
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import           Luna.Studio.State.Global           (State)
import            qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor



expandSelectedNodes :: Command State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ Node.isExpanded) sn
        update      = if allSelected then Node.isExpanded %~ not
                                     else Node.isExpanded .~ True
    Global.withNodeEditor $ forM_ sn $ \node -> do
        NodeEditor.nodes . at (node ^. Node.nodeId) %= fmap update
    updateConnectionsForNodes $ map (view Node.nodeId) sn
