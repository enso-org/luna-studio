{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateNodeValue where

import Common.Prelude


import qualified Data.Map                                   as Map
import qualified Data.Text                                  as Text
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node


import Common.Action.Command                      (Command)
import JS.Visualizers                             (notifyStreamRestart, sendInternalData, sendStreamDatapoint, sendVisualizationData)
import LunaStudio.Data.Error                      (errorContent)
import LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue))
import LunaStudio.Data.TypeRep                    (toConstructorRep)
import NodeEditor.Action.State.NodeEditor         (getExpressionNodeType, getNodeVisualizations, getVisualizationBackup, getVisualizersForType, 
                                                   modifyExpressionNode, modifyNodeEditor, recoverVisualizations, setErrorVisualization,
                                                   setPlaceholderVisualization, updateVisualizationsForNode)
import NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), execTime, value)
import NodeEditor.React.Model.NodeEditor          (VisualizationBackup (ErrorBackup, MessageBackup, StreamBackup, ValueBackup), backupMap,
                                                   visualizationsBackup, _StreamBackup)
import NodeEditor.React.Model.Visualization       (VisualizationValue (StreamDataPoint, StreamStart, Value), noDataMsg, noVisMsg, visualizations,
                                                   visualizations)
import NodeEditor.State.Global                    (State)


import NodeEditor.Action.State.NodeEditor         (getExpressionNode)

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

isNewData :: NodeLoc -> VisualizationBackup -> Command State Bool
isNewData nl vp = (Just vp /=) <$> getVisualizationBackup nl 

setVisualizationData :: NodeLoc -> VisualizationBackup -> Bool -> Command State ()
setVisualizationData nl backup@(ValueBackup val) overwrite = whenM ((overwrite ||) <$> isNewData nl backup) $ do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- updateVisualizationsForNode nl
    withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
        liftIO . forM_ visIds $ \visId -> sendVisualizationData visId cRep val
setVisualizationData nl backup@(StreamBackup values) overwrite@True = whenM (isNewData nl backup) $ do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- updateVisualizationsForNode nl
    withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl) $ \cRep ->
        liftIO . forM_ visIds $ \visId -> notifyStreamRestart visId cRep $ reverse values
setVisualizationData nl backup@(StreamBackup values) overwrite@False = do
    modifyNodeEditor $ visualizationsBackup . backupMap . ix nl . _StreamBackup %= (values <>)
    visIds <- maybe def (Map.keys . view visualizations) <$> getNodeVisualizations nl
    liftIO . forM_ visIds $ forM_ values . sendStreamDatapoint
setVisualizationData nl backup@(MessageBackup msg) overwrite = whenM ((overwrite ||) <$> isNewData nl backup) $ do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- setPlaceholderVisualization nl
    liftIO . forM_ visIds $ flip sendInternalData msg
setVisualizationData nl backup@(ErrorBackup msg) overwrite = whenM ((overwrite ||) <$> isNewData nl backup) $ do
    modifyNodeEditor $ visualizationsBackup . backupMap . at nl ?= backup
    visIds <- setErrorVisualization nl
    liftIO . forM_ visIds $ flip sendInternalData msg

