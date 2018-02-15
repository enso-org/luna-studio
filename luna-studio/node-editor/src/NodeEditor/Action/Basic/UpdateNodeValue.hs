{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Map                                   as Map
import qualified Data.Text                                  as Text
import           JS.Visualizers                             (notifyStreamRestart, sendInternalData, sendStreamDatapoint,
                                                             sendVisualizationData)
import           LunaStudio.Data.Error                      (errorContent)
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue),
                                                             VisualizationValue (StreamDataPoint, StreamStart, Value))
import           LunaStudio.Data.TypeRep                    (toConstructorRep)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodeType, getNodeVisualizations, getVisualizersForType,
                                                             modifyExpressionNode, modifyNodeEditor, recoverVisualizations,
                                                             setErrorVisualization, setPlaceholderVisualization,
                                                             updateVisualizationsForNode)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), execTime, value)
import           NodeEditor.React.Model.NodeEditor          (VisualizationBackup (ErrorBackup, MessageBackup, StreamBackup, ValueBackup),
                                                             backupMap, visualizationsBackup, _StreamBackup)
import           NodeEditor.React.Model.Visualization       (noDataMsg, noVisMsg, visualizations)
import           NodeEditor.State.Global                    (State)

import           NodeEditor.Action.State.NodeEditor         (getExpressionNode)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node


updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl = \case
    NodeValue sv (Just (StreamDataPoint visVal)) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        setVisualizationData nl (StreamBackup [visVal]) False
    NodeValue sv (Just (Value visVal)) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        setVisualizationData nl (ValueBackup visVal) True
    NodeValue sv (Just StreamStart) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        setVisualizationData nl (StreamBackup []) True
    NodeValue sv Nothing -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        hasVisualizers <- maybe (return False) (fmap isJust . getVisualizersForType) =<< getExpressionNodeType nl
        let msg = if hasVisualizers then noDataMsg else noVisMsg
        setVisualizationData nl (MessageBackup msg) True
    NodeError e -> do
        modifyExpressionNode nl $ value .= Error e
        setVisualizationData nl (ErrorBackup $ e ^. errorContent) True


setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t

setVisualizationData :: NodeLoc -> VisualizationBackup -> Bool -> Command State ()
setVisualizationData nl backup@(ValueBackup val) _ = do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- updateVisualizationsForNode nl
    withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
        liftIO . forM_ visIds $ \visId -> sendVisualizationData visId cRep val
setVisualizationData nl backup@(StreamBackup values) override@True = do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- updateVisualizationsForNode nl
    withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
        liftIO . forM_ visIds $ \visId -> notifyStreamRestart visId cRep $ reverse values
setVisualizationData nl backup@(StreamBackup values) override@False = do
    modifyNodeEditor $ visualizationsBackup . backupMap . ix nl . _StreamBackup %= (values <>)
    visIds <- maybe def (Map.keys . view visualizations) <$> getNodeVisualizations nl
    liftIO . forM_ visIds $ forM_ values . sendStreamDatapoint
setVisualizationData nl backup@(MessageBackup msg) _ = do
    print ("SET VIS DATA", backup)
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- setPlaceholderVisualization nl
    liftIO . forM_ visIds $ flip sendInternalData msg
setVisualizationData nl backup@(ErrorBackup msg) _ = do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- setErrorVisualization nl
    liftIO . forM_ visIds $ flip sendInternalData msg

