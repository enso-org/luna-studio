module Luna.Studio.Action.Basic.RemoveNode where

import           Control.Monad                             (filterM)
import qualified Data.Set                                  as Set
import           Empire.API.Data.NodeLoc                   (NodeLoc)
import qualified JS.GoogleAnalytics                        as GA
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnectionsContainingNodes)
import           Luna.Studio.Action.Basic.SelectNode       (selectPreviousNodes)
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.State.NodeEditor       (getSelectedNodes, inGraph)
import qualified Luna.Studio.Action.State.NodeEditor       as NodeEditor
import           Luna.Prelude
import           Luna.Studio.React.Model.Node              (nodeLoc)
import           Luna.Studio.State.Global                  (State)


removeNode :: NodeLoc -> Command State ()
removeNode = removeNodes . return

removeNodes :: [NodeLoc] -> Command State ()
removeNodes nls = do
    removedNodes <- localRemoveNodes nls
    Batch.removeNodes removedNodes
    GA.sendEvent $ GA.RemoveNode $ length removedNodes

removeSelectedNodes :: Command State ()
removeSelectedNodes = getSelectedNodes >>= removeNodes . map (view nodeLoc)

localRemoveSelectedNodes :: Command State [NodeLoc]
localRemoveSelectedNodes = getSelectedNodes >>= localRemoveNodes . map (view nodeLoc)

localRemoveNode :: NodeLoc -> Command State (Maybe NodeLoc)
localRemoveNode = fmap listToMaybe . localRemoveNodes . return

localRemoveNodes :: [NodeLoc] -> Command State [NodeLoc]
localRemoveNodes nodeLocs = do
    nls <- filterM inGraph nodeLocs
    void $ localRemoveConnectionsContainingNodes nls
    mapM_ NodeEditor.removeNode nls
    selectedIds <- Set.fromList . (map (view nodeLoc)) <$> getSelectedNodes
    when (Set.isSubsetOf selectedIds $ Set.fromList nls) selectPreviousNodes
    return nls
