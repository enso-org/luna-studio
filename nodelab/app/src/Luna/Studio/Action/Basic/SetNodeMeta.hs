module Luna.Studio.Action.Basic.SetNodeMeta where

import           Control.Monad                           (filterM)
import           Data.Position                           (Position)
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import qualified Luna.Studio.Action.Batch                as Batch
import           Luna.Studio.Action.Command              (Command)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (NodeId)
import qualified Luna.Studio.React.Model.Node            as Node
import           Luna.Studio.State.Global                (State)


toggleVisualizations :: NodeId -> Bool -> Command State ()
toggleVisualizations nid displayRes = do
    mayPos <- view Node.position <∘> NodeEditor.getNode nid
    withJust mayPos $ \pos -> setNodesMeta [(nid, pos, displayRes)]

localToggleVisualizations :: NodeId -> Bool -> Command State ()
localToggleVisualizations nid displayRes = do
    mayPos <- view Node.position <∘> NodeEditor.getNode nid
    withJust mayPos $ \pos -> void $ localSetNodesMeta [(nid, pos, displayRes)]

moveNode :: (NodeId, Position) -> Command State ()
moveNode = moveNodes . return

localMoveNode :: (NodeId, Position) -> Command State Bool
localMoveNode = fmap (not . null) . localMoveNodes . return

moveNodes :: [(NodeId, Position)] -> Command State ()
moveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nid, pos) ->
        flip fmap2 (NodeEditor.getNode nid) $
            \node -> (nid, pos, node ^. Node.visualizationsEnabled)
    setNodesMeta update

localMoveNodes :: [(NodeId, Position)] -> Command State [NodeId]
localMoveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nid, pos) ->
        flip fmap2 (NodeEditor.getNode nid) $
            \node -> (nid, pos, node ^. Node.visualizationsEnabled)
    localSetNodesMeta update

setNodeMeta :: (NodeId, Position, Bool) -> Command State ()
setNodeMeta = setNodesMeta . return

setNodesMeta :: [(NodeId, Position, Bool)] -> Command State ()
setNodesMeta update' = filterM (uncurry localSetNodeMeta) update' >>= \update ->
    unless (null update) $ Batch.setNodesMeta update

localSetNodesMeta :: [(NodeId, Position, Bool)] -> Command State [NodeId]
localSetNodesMeta = fmap2 (view _1) . filterM (\(nid, pos, dispRes) -> localSetNodeMeta nid pos dispRes)

localSetNodeMeta :: NodeId -> Position -> Bool -> Command State Bool
localSetNodeMeta nid pos dispRes = do
    NodeEditor.modifyNode nid $ do
        Node.visualizationsEnabled .= dispRes
        Node.position              .= pos
    void $ redrawConnectionsForNode nid
    NodeEditor.inGraph nid
