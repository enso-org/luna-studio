module Luna.Studio.Action.Basic.RemoveNode where

import           Control.Monad                             (filterM)
import qualified Data.Set                                  as Set
import           Empire.API.Data.Node                      (NodeId)
import qualified JS.GoogleAnalytics                        as GA
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnectionsContainingNodes)
import           Luna.Studio.Action.Basic.SelectNode       (selectPreviousNodes)
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.State.Graph            (inGraph)
import qualified Luna.Studio.Action.State.Graph            as Graph
import           Luna.Studio.Action.State.NodeEditor       (getSelectedNodes)
import qualified Luna.Studio.Action.State.NodeEditor       as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node              (nodeId)
import           Luna.Studio.State.Global                  (State)


removeNode :: NodeId -> Command State ()
removeNode = removeNodes . return

removeNodes :: [NodeId] -> Command State ()
removeNodes nids = do
    removedNodes <- localRemoveNodes nids
    Batch.removeNodes removedNodes
    GA.sendEvent $ GA.RemoveNode $ length removedNodes

removeSelectedNodes :: Command State ()
removeSelectedNodes = getSelectedNodes >>= removeNodes . map (view nodeId)

localRemoveSelectedNodes :: Command State [NodeId]
localRemoveSelectedNodes = getSelectedNodes >>= localRemoveNodes . map (view nodeId)

localRemoveNode :: NodeId -> Command State (Maybe NodeId)
localRemoveNode = fmap listToMaybe . localRemoveNodes . return

localRemoveNodes :: [NodeId] -> Command State [NodeId]
localRemoveNodes nodeIds = do
    nids <- filterM inGraph nodeIds
    void $ localRemoveConnectionsContainingNodes nids
    mapM_ Graph.removeNode      nids
    mapM_ NodeEditor.removeNode nids
    selectedIds <- Set.fromList . (map (view nodeId)) <$> getSelectedNodes
    when (Set.isSubsetOf selectedIds $ Set.fromList nids) selectPreviousNodes
    return nids
