module Luna.Studio.Action.Node.Remove
    ( removeSelectedNodes
    , localRemoveNodes
    ) where

import qualified Data.Set                            as Set
import           Empire.API.Data.Node                (NodeId)
import qualified JS.GoogleAnalytics                  as GA
import qualified Luna.Studio.Action.Batch            as BatchCmd
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Graph.Disconnect (localRemoveConnections)
import           Luna.Studio.Action.Graph.Selection  (selectPreviousNodes, selectedNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor  as NodeEditor
import           Luna.Studio.State.Global            (State)
import qualified Luna.Studio.State.Global            as Global
import qualified Luna.Studio.State.Graph             as Graph


removeSelectedNodes :: Command State ()
removeSelectedNodes = do
    selected <- selectedNodes
    performRemoval $ view NodeModel.nodeId <$> selected
    selectPreviousNodes

performRemoval :: [NodeId] -> Command State ()
performRemoval nodeIds = do
    BatchCmd.removeNodes nodeIds
    GA.sendEvent $ GA.RemoveNode $ length nodeIds

localRemoveNodes :: [NodeId] -> Command State ()
localRemoveNodes nodeIds = do
    selectedNodesIds <- map (view NodeModel.nodeId) <$> selectedNodes
    let selectPrevious =  Set.isSubsetOf (Set.fromList selectedNodesIds) $ Set.fromList nodeIds
    danglingConns <- concat <$> forM nodeIds (uses Global.graph . Graph.connectionIdsContainingNode)
    localRemoveConnections danglingConns
    forM_ nodeIds $ \nodeId -> Global.graph %= Graph.removeNode nodeId
    Global.modifyNodeEditor $
        forM_ nodeIds $ \nodeId ->
            NodeEditor.nodes . at nodeId .= Nothing
    when selectPrevious selectPreviousNodes
