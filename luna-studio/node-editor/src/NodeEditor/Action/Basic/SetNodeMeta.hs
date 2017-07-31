module NodeEditor.Action.Basic.SetNodeMeta where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Monad                              (filterM)
import           LunaStudio.Data.NodeMeta                   (NodeMeta (NodeMeta))
import qualified LunaStudio.Data.NodeMeta                   as NodeMeta
import           LunaStudio.Data.Position                   (Position)
import qualified NodeEditor.Action.Batch                    as Batch
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, defaultVisualizer, position, visualizationsEnabled)
import           NodeEditor.State.Global                    (State)

moveNode :: (NodeLoc, Position) -> Command State ()
moveNode = moveNodes . return

localMoveNode :: (NodeLoc, Position) -> Command State Bool
localMoveNode = fmap (not . null) . localMoveNodes . return

moveNodes :: [(NodeLoc, Position)] -> Command State ()
moveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nl, pos) ->
        flip fmap2 (NodeEditor.getExpressionNode nl) $
            \node -> (nl, NodeMeta pos (node ^. visualizationsEnabled) (node ^. defaultVisualizer))
    setNodesMeta update

localMoveNodes :: [(NodeLoc, Position)] -> Command State [NodeLoc]
localMoveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nl, pos) ->
        flip fmap2 (NodeEditor.getExpressionNode nl) $
            \node -> (nl, NodeMeta pos (node ^. visualizationsEnabled) (node ^. defaultVisualizer))
    localSetNodesMeta update

setNodeMeta :: (NodeLoc, NodeMeta) -> Command State ()
setNodeMeta = setNodesMeta . return

setNodesMeta :: [(NodeLoc, NodeMeta)] -> Command State ()
setNodesMeta update' = filterM (uncurry localSetNodeMeta) update' >>= \update ->
    unless (null update) $ Batch.setNodesMeta update

-- WARNING: Those functions does not affect defaultVisualizer because it is set
--          in NodeMeta so we could inform backend about change for backup.
--          GUIhas fresher data and we don't set visualizer via setNodeMeta.
localSetNodesMeta :: [(NodeLoc, NodeMeta)] -> Command State [NodeLoc]
localSetNodesMeta = fmap2 (view _1) . filterM (uncurry localSetNodeMeta)

localSetNodeMeta :: NodeLoc -> NodeMeta -> Command State Bool
localSetNodeMeta nl nm = do
    NodeEditor.modifyExpressionNode nl $ do
        visualizationsEnabled .= nm ^. NodeMeta.displayResult
        position              .= nm ^. NodeMeta.position
    NodeEditor.inGraph nl

-- Use this function to notify backend - this function prevents undo
sendNodesMetaUpdate :: [(NodeLoc, NodeMeta)] -> Command State ()
sendNodesMetaUpdate = Batch.sendNodesMetaUpdate
