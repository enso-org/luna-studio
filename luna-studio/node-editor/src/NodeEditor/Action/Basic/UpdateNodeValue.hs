module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Prelude
import           JS.Visualizers                             (registerVisualizerFrame, sendVisualisationData)
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue, StreamDataPoint),
                                                             VisualizationValue (StreamStart, Value))
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), activeVisualizer, execTime,
                                                             getActiveVisualization, value, visualization, visualizationId)
import           NodeEditor.State.Global                    (State)


updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl nv = case nv of
    NodeValue sv (Just (Value visVal)) -> do
        uuid <- getUUID
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            vis <- use visualization
            when (isJust $ vis ^. activeVisualizer) $
                visualization . visualizationId ?= uuid
        whenM (isJust . maybe def getActiveVisualization <$> getExpressionNode nl) . liftIO $ do
            registerVisualizerFrame uuid
            sendVisualisationData uuid visVal
    NodeValue sv (Just StreamStart) -> do
        uuid <- getUUID
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            vis <- use visualization
            when (isJust $ vis ^. activeVisualizer) $
                visualization . visualizationId ?= uuid
        whenM (isJust . maybe def getActiveVisualization <$> getExpressionNode nl) . liftIO $
            registerVisualizerFrame uuid
    NodeValue sv Nothing -> modifyExpressionNode nl $ do
        value ?= ShortValue sv
        visualization . visualizationId .= def
    StreamDataPoint visData -> withJustM (getExpressionNode nl) $ \n ->
        withJust (getActiveVisualization n) $ liftIO . flip sendVisualisationData visData . fst
    NodeError e -> modifyExpressionNode nl $ do
        value ?= Error e
        visualization . visualizationId .= def


setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
