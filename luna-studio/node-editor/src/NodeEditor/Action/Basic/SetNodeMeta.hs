module NodeEditor.Action.Basic.SetNodeMeta where

import           Common.Prelude

import qualified LunaStudio.Data.NodeMeta                   as NodeMeta
import qualified LunaStudio.Data.Project                    as Project
import qualified NodeEditor.Action.Batch                    as Batch
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor

import           Common.Action.Command                      (Command)
import           Control.Monad                              (filterM)
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           LunaStudio.Data.NodeMeta                   (NodeMeta (NodeMeta))
import           LunaStudio.Data.Position                   (Position)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, defaultVisualizer, position, visEnabled)
import           NodeEditor.State.Global                    (State)


moveNode :: NodeLoc -> Position -> Command State ()
moveNode = moveNodes .: Map.singleton

localMoveNode :: NodeLoc -> Position -> Command State Bool
localMoveNode = fmap (not . null) . localMoveNodes .: Map.singleton

toMetaUpdate :: Map NodeLoc Position -> Command State (Map NodeLoc NodeMeta)
toMetaUpdate
    = fmap (Map.fromList . catMaybes) . mapM (uncurry toMeta) . Map.toList where
        newMeta pos n = NodeMeta
            pos
            (n ^. visEnabled)
            (Project.toOldAPI <$> n ^. defaultVisualizer)
        toMeta nl pos = fmap ((nl,) . newMeta pos)
            <$> NodeEditor.getExpressionNode nl


moveNodes :: Map NodeLoc Position -> Command State ()
moveNodes updates = setNodesMeta =<< toMetaUpdate updates

localMoveNodes :: Map NodeLoc Position -> Command State [NodeLoc]
localMoveNodes updates = localSetNodesMeta =<< toMetaUpdate updates

setNodeMeta :: NodeLoc -> NodeMeta -> Command State ()
setNodeMeta = setNodesMeta .: Map.singleton

setNodesMeta :: Map NodeLoc NodeMeta -> Command State ()
setNodesMeta update' = filterM (uncurry localSetNodeMeta) (Map.toList update')
    >>= \update -> unless (null update)
        . Batch.setNodesMeta $ Map.fromList update

-- WARNING: Those functions does not affect defaultVisualizer because it is set
--          in NodeMeta so we could inform backend about change for backup.
--          GUIhas fresher data and we don't set visualizer via setNodeMeta.
localSetNodesMeta :: Map NodeLoc NodeMeta -> Command State [NodeLoc]
localSetNodesMeta
    = fmap2 (view _1) . filterM (uncurry localSetNodeMeta) . Map.toList

localSetNodeMeta :: NodeLoc -> NodeMeta -> Command State Bool
localSetNodeMeta nl nm = do
    NodeEditor.modifyExpressionNode nl $ do
        visEnabled .= nm ^. NodeMeta.displayResult
        position   .= nm ^. NodeMeta.position
    NodeEditor.inGraph nl

-- Use this function to notify backend - this function prevents undo
sendNodesMetaUpdate :: Map NodeLoc NodeMeta -> Command State ()
sendNodesMetaUpdate = Batch.sendNodesMetaUpdate
