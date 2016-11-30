module Reactive.Commands.Node.Remove
    ( removeSelectedNodes
    , localRemoveNodes
    ) where

import           React.Store                        (widget)
import qualified React.Store                        as Store
import qualified React.Store.NodeEditor             as NodeEditor
import           Reactive.Commands.Command          (Command)
import           Reactive.Commands.Graph.Disconnect (localDisconnectAll)
import qualified Reactive.Commands.Graph.Selection  as Selection
import           Reactive.State.Global              (State)
import qualified Reactive.State.Global              as Global
import qualified Reactive.State.Graph               as Graph
import           Utils.PreludePlus

import           Empire.API.Data.Node               (NodeId)
import qualified Object.Widget.Node                 as NodeModel
import qualified Reactive.Commands.Batch            as BatchCmd

import qualified JS.GoogleAnalytics                 as GA


removeSelectedNodes :: Command State ()
removeSelectedNodes = do
    selectedNodes <- Selection.selectedNodes
    performRemoval $ (^. widget . NodeModel.nodeId) <$> selectedNodes

performRemoval :: [NodeId] -> Command State ()
performRemoval nodeIds = do
    BatchCmd.removeNodes nodeIds
    GA.sendEvent $ GA.RemoveNode $ length nodeIds

localRemoveNodes :: [NodeId] -> Command State ()
localRemoveNodes = mapM_ $ \nodeId -> do
    danglingConns <- uses Global.graph $ Graph.connectionIdsContainingNode nodeId
    localDisconnectAll danglingConns
    Global.graph %= Graph.removeNode nodeId
    Global.inNodeEditor $ Store.modify_ $
        NodeEditor.nodes . at nodeId .~ Nothing
