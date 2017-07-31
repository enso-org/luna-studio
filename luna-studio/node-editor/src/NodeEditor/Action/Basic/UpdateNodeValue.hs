module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Map                                   as Map
import qualified Data.Text                                  as Text
import           JS.Visualizers                             (notifyStreamRestart, sendStreamDatapoint, sendVisualizationData)
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue),
                                                             VisualizationValue (StreamDataPoint, StreamStart, Value))
import           LunaStudio.Data.TypeRep                    (toConstructorRep)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodeType, getNodeVisualizations, modifyExpressionNode,
                                                             modifyNodeEditor, recoverVisualizations, updateVisualizationsForNode)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), execTime, value)
import           NodeEditor.React.Model.NodeEditor          (VisualizationBackup (StreamBackup, ValueBackup), backupMap,
                                                             visualizationsBackup)
import           NodeEditor.React.Model.Visualization       (visualizations)
import           NodeEditor.State.Global                    (State)


updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl = \case
    NodeValue sv (Just (StreamDataPoint visVal)) -> do
        modifyExpressionNode nl $ value ?= ShortValue (Text.take 100 sv)
        let updateBackup (StreamBackup backup) = Just . StreamBackup $ visVal : backup
            updateBackup _                     = Nothing
        modifyNodeEditor $ visualizationsBackup . backupMap %= Map.update updateBackup nl
        visIds <- maybe def (Map.keys . view visualizations) <$> getNodeVisualizations nl
        liftIO . forM_ visIds $ flip sendStreamDatapoint visVal
    NodeValue sv (Just (Value visVal)) -> do
        modifyExpressionNode nl $ value ?= ShortValue (Text.take 100 sv)
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= ValueBackup visVal
        visIds <- recoverVisualizations nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
            liftIO . forM_ visIds $ \visId -> sendVisualizationData visId cRep visVal
    NodeValue sv (Just StreamStart) -> do
        modifyExpressionNode nl $ value ?= ShortValue (Text.take 100 sv)
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= StreamBackup []
        visIds <- recoverVisualizations nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
            liftIO . forM_ visIds $ \visId -> notifyStreamRestart visId cRep def
    NodeValue sv Nothing -> do
        modifyExpressionNode nl $ value ?= ShortValue (Text.take 100 sv)
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl .= def
        updateVisualizationsForNode nl Nothing
    NodeError e -> do
        modifyExpressionNode nl $ value ?= Error e
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl .= def
        updateVisualizationsForNode nl Nothing

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
