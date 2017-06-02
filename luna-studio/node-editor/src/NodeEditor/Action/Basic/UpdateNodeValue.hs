module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Prelude
import           JS.Visualizers                             (notifyStreamRestart, registerVisualizerFrame, sendStreamDatapoint,
                                                             sendVisualisationData)
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue),
                                                             VisualizationValue (StreamDataPoint, StreamStart, Value))
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode, modifyNodeEditor)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), execTime, getVisualization, isActive,
                                                             value, visualization, visualizationId)
import           NodeEditor.React.Model.NodeEditor          (backupMap, visualizationsBackup)
import           NodeEditor.State.Global                    (State)


updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl nv = case nv of
    NodeValue sv (Just (StreamDataPoint visVal)) -> do
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
        withJustM (maybe def getVisualization <$> getExpressionNode nl) $
            liftIO . flip sendStreamDatapoint visVal . view _1
    NodeValue sv (Just vv@(Value visVal)) -> do
        uuid <- getUUID
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= vv
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            visualization . _Just . visualizationId %= Just . fromMaybe uuid
        withJustM (maybe def getVisualization <$> getExpressionNode nl) $ \(visId, _, _) -> liftIO $ do
            when (uuid == visId) $ registerVisualizerFrame visId
            sendVisualisationData visId visVal
    NodeValue sv (Just vv@StreamStart) -> do
        uuid <- getUUID
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= vv
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            visualization . _Just . visualizationId %= Just . fromMaybe uuid
        withJustM (maybe def getVisualization <$> getExpressionNode nl) $ \(visId, _, _) -> liftIO $ do
            when (uuid == visId) $ registerVisualizerFrame visId
            notifyStreamRestart visId
    NodeValue sv Nothing -> do
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl .= def
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            visualization . _Just . visualizationId .= def
            visualization . _Just . isActive        .= False
    NodeError e -> do
        modifyNodeEditor $ visualizationsBackup . backupMap . at nl .= def
        modifyExpressionNode nl $ do
            value ?= Error e
            visualization . _Just . visualizationId .= def
            visualization . _Just . isActive        .= False

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
