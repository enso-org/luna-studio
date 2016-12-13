module Reactive.Commands.Node.Remove
    ( removeSelectedNodes
    , localRemoveNodes
    ) where

import qualified Data.Set                           as Set
import           Luna.Studio.React.Store                        (widget)
import qualified Luna.Studio.React.Store                        as Store
import qualified Luna.Studio.React.Model.NodeEditor             as NodeEditor
import           Reactive.Commands.Command          (Command)
import           Reactive.Commands.Graph.Disconnect (localDisconnectAll)
import           Reactive.Commands.Graph.Selection  (selectPreviousNodes, selectedNodes)
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph
import           Luna.Studio.Prelude

import           Empire.API.Data.Node               (NodeId)
import qualified Object.Widget.Node                 as NodeModel
import qualified Reactive.Commands.Batch            as BatchCmd

import qualified JS.GoogleAnalytics                 as GA


removeSelectedNodes :: Command State ()
removeSelectedNodes = do
    selected <- selectedNodes
    performRemoval $ (^. widget . NodeModel.nodeId) <$> selected
    selectPreviousNodes

performRemoval :: [NodeId] -> Command State ()
performRemoval nodeIds = do
    BatchCmd.removeNodes nodeIds
    GA.sendEvent $ GA.RemoveNode $ length nodeIds

localRemoveNodes :: [NodeId] -> Command State ()
localRemoveNodes nodeIds = forM_ nodeIds $ \nodeId -> do
    selectedNodesIds <- map (^. widget . NodeModel.nodeId) <$> selectedNodes
    let selectPrevious =  Set.isSubsetOf (Set.fromList selectedNodesIds) $ Set.fromList nodeIds
    danglingConns <- uses Global.graph $ Graph.connectionIdsContainingNode nodeId
    localDisconnectAll danglingConns
    Global.graph %= Graph.removeNode nodeId
    Global.withNodeEditor $ Store.modify_ $
        NodeEditor.nodes . at nodeId .~ Nothing
    when selectPrevious selectPreviousNodes
