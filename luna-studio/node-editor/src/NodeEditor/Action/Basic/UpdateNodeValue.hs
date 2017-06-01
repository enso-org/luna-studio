module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Prelude
import           JS.Visualizers                             (registerVisualizerFrame, sendVisualisationData)
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue, StreamDataPoint),
                                                             VisualizationValue (StreamStart, Value))
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), execTime, getVisualization, isActive,
                                                             value, visualization, visualizationId)
import           NodeEditor.State.Global                    (State)


updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl nv = case nv of
    NodeValue sv (Just (Value visVal)) -> do
        uuid <- getUUID
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            visualization . _Just . visualizationId ?= uuid
        whenM (isJust . maybe def getVisualization <$> getExpressionNode nl) . liftIO $ do
            registerVisualizerFrame uuid
            sendVisualisationData uuid visVal
    NodeValue sv (Just StreamStart) -> do
        uuid <- getUUID
        modifyExpressionNode nl $ do
            value ?= ShortValue sv
            visualization . _Just . visualizationId ?= uuid
        whenM (isJust . maybe def getVisualization <$> getExpressionNode nl) . liftIO $
            registerVisualizerFrame uuid
    NodeValue sv Nothing -> modifyExpressionNode nl $ do
        value ?= ShortValue sv
        visualization . _Just . visualizationId .= def
        visualization . _Just . isActive        .= False
    StreamDataPoint visData -> withJustM (maybe def getVisualization <$> getExpressionNode nl) $
        liftIO . flip sendVisualisationData visData . view _1
    NodeError e -> modifyExpressionNode nl $ do
        value ?= Error e
        visualization . _Just . visualizationId .= def
        visualization . _Just . isActive        .= False

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
