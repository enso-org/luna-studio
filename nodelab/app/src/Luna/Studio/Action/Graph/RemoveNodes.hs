module Luna.Studio.Action.Graph.RemoveNodes where

import qualified Data.HashMap.Strict                       as HashMap
import qualified Data.Set                                  as Set
import           Empire.API.Data.Node                      (NodeId)
import qualified JS.GoogleAnalytics                        as GA
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.Graph.RemoveConnection (localRemoveConnection)
import           Luna.Studio.Action.Graph.Selection        (selectPreviousNodes, selectedNodeIds)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor        as NodeEditor
import           Luna.Studio.State.Global                  (State)
import qualified Luna.Studio.State.Global                  as Global
import qualified Luna.Studio.State.Graph                   as Graph


removeSelectedNodes :: Command State ()
removeSelectedNodes = selectedNodeIds >>= removeNodes

removeNode :: NodeId -> Command State ()
removeNode nodeId = removeNodes [nodeId]

removeNodes :: [NodeId] -> Command State ()
removeNodes nodeIds = do
    removedNodes <- localRemoveNodes nodeIds
    Batch.removeNodes removedNodes
    GA.sendEvent $ GA.RemoveNode $ length removedNodes

localRemoveNode :: NodeId -> Command State (Maybe NodeId)
localRemoveNode nodeId = listToMaybe <$> localRemoveNodes [nodeId]

localRemoveNodes :: [NodeId] -> Command State [NodeId]
localRemoveNodes nodeIds' = do
    nodesMap <- use $ Global.graph . Graph.nodesMap
    let nodeIds = filter (flip HashMap.member nodesMap) nodeIds'
    selectedIds <- selectedNodeIds
    let selectPrevious =  Set.isSubsetOf (Set.fromList selectedIds) $ Set.fromList nodeIds
    danglingConns <- concat <$> forM nodeIds (uses Global.graph . Graph.connectionIdsContainingNode)
    mapM_ localRemoveConnection danglingConns
    forM_ nodeIds $ \nodeId -> Global.graph %= Graph.removeNode nodeId
    Global.modifyNodeEditor $ forM_ nodeIds $ \nodeId ->
        NodeEditor.nodes . at nodeId .= Nothing
    when selectPrevious selectPreviousNodes
    return nodeIds
