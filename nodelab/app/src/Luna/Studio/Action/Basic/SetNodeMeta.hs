module Luna.Studio.Action.Basic.SetNodeMeta where

import           Control.Monad                           (filterM)
import           Data.Position                           (Position, fromTuple, toTuple)
import           Empire.API.Data.Node                    (NodeId, nodeMeta)
import qualified Empire.API.Data.Node                    as Node
import           Empire.API.Data.NodeMeta                (NodeMeta (NodeMeta), displayResult, position)
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import qualified Luna.Studio.Action.Batch                as Batch
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.Graph          (inGraph)
import qualified Luna.Studio.Action.State.Graph          as Graph
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (visualizationsEnabled)
import qualified Luna.Studio.React.Model.Node            as Model
import           Luna.Studio.State.Global                (State)


toggleVisualizations :: NodeId -> Bool -> Command State ()
toggleVisualizations nid displayRes = do
    mayPos <- (fmap . fmap) (view Node.position) $ Graph.getNode nid
    withJust mayPos $ \pos -> setNodesMeta [(nid, NodeMeta pos displayRes)]

localToggleVisualizations :: NodeId -> Bool -> Command State ()
localToggleVisualizations nid displayRes = do
    mayPos <- (fmap . fmap) (view Node.position) $ Graph.getNode nid
    withJust mayPos $ \pos -> void $ localSetNodesMeta [(nid, NodeMeta pos displayRes)]

moveNode :: (NodeId, Position) -> Command State ()
moveNode = moveNodes . return

localMoveNode :: (NodeId, Position) -> Command State Bool
localMoveNode = fmap (not . null) . localMoveNodes . return

moveNodes :: [(NodeId, Position)] -> Command State ()
moveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nid, pos) ->
        flip (fmap . fmap) (Graph.getNode nid) $
            \node -> (nid, NodeMeta (toTuple pos) (node ^. nodeMeta . displayResult))
    setNodesMeta update

localMoveNodes :: [(NodeId, Position)] -> Command State [NodeId]
localMoveNodes nodesPos = do
    update <- fmap catMaybes . forM nodesPos $ \(nid, pos) ->
        flip (fmap . fmap) (Graph.getNode nid) $
            \node -> (nid, NodeMeta (toTuple pos) (node ^. nodeMeta . displayResult))
    localSetNodesMeta update

setNodeMeta :: (NodeId, NodeMeta) -> Command State ()
setNodeMeta = setNodesMeta . return

setNodesMeta :: [(NodeId, NodeMeta)] -> Command State ()
setNodesMeta update' = filterM (uncurry localSetNodeMeta) update' >>= \update ->
    unless (null update) $ Batch.setNodesMeta update

localSetNodesMeta :: [(NodeId, NodeMeta)] -> Command State [NodeId]
localSetNodesMeta = (fmap . map) fst . filterM (uncurry localSetNodeMeta)

localSetNodeMeta :: NodeId -> NodeMeta -> Command State Bool
localSetNodeMeta nid update = do
    Graph.modifyNode      nid $ nodeMeta .~ update
    NodeEditor.modifyNode nid $ do
        visualizationsEnabled .= update ^. displayResult
        Model.position        .= (fromTuple $ update ^. position)
    void $ redrawConnectionsForNode nid
    inGraph nid
