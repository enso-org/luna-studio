module Luna.Studio.Action.Basic.SetNodeMeta where

import           Control.Monad                               (filterM)
import           Data.Position                               (Position)
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, position, visualizationsEnabled)
import           Luna.Studio.State.Global                    (State)


toggleVisualizations :: NodeLoc -> Bool -> Command State ()
toggleVisualizations nl displayRes = do
    mayPos <- view position <∘> NodeEditor.getExpressionNode nl
    withJust mayPos $ \pos -> setNodesMeta [(nl, pos, displayRes)]

localToggleVisualizations :: NodeLoc -> Bool -> Command State ()
localToggleVisualizations nl displayRes = do
    mayPos <- view position <∘> NodeEditor.getExpressionNode nl
    withJust mayPos $ \pos -> void $ localSetNodesMeta [(nl, pos, displayRes)]

moveNode :: (NodeLoc, Position) -> Command State ()
moveNode = moveNodes . return

localMoveNode :: (NodeLoc, Position) -> Command State Bool
localMoveNode = fmap (not . null) . localMoveNodes . return

moveNodes :: [(NodeLoc, Position)] -> Command State ()
moveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nl, pos) ->
        flip fmap2 (NodeEditor.getExpressionNode nl) $
            \node -> (nl, pos, node ^. visualizationsEnabled)
    setNodesMeta update

localMoveNodes :: [(NodeLoc, Position)] -> Command State [NodeLoc]
localMoveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nl, pos) ->
        flip fmap2 (NodeEditor.getExpressionNode nl) $
            \node -> (nl, pos, node ^. visualizationsEnabled)
    localSetNodesMeta update

setNodeMeta :: (NodeLoc, Position, Bool) -> Command State ()
setNodeMeta = setNodesMeta . return

setNodesMeta :: [(NodeLoc, Position, Bool)] -> Command State ()
setNodesMeta update' = filterM (uncurry localSetNodeMeta) update' >>= \update ->
    unless (null update) $ Batch.setNodesMeta update

localSetNodesMeta :: [(NodeLoc, Position, Bool)] -> Command State [NodeLoc]
localSetNodesMeta = fmap2 (view _1) . filterM (\(nl, pos, dispRes) -> localSetNodeMeta nl pos dispRes)

localSetNodeMeta :: NodeLoc -> Position -> Bool -> Command State Bool
localSetNodeMeta nl pos dispRes = do
    NodeEditor.modifyExpressionNode nl $ do
        visualizationsEnabled .= dispRes
        position              .= pos
    void $ redrawConnectionsForNode nl
    NodeEditor.inGraph nl
