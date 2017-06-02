{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Visualization where

import           Common.Prelude
import qualified Data.Map                                   as Map
import           JS.Visualizers                             (notifyStreamRestart, registerVisualizerFrame, sendVisualisationData)
import           LunaStudio.Data.NodeLoc                    (NodeLoc)
import           LunaStudio.Data.NodeValue                  (VisualizationValue (StreamStart, Value), VisualizerName)
import           LunaStudio.Data.Position                   (Position)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getSelectedNodes, getVisualizationsBackupMap,
                                                             modifyExpressionNode, modifyNodeEditor)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.Event.Mouse                     (workspacePosition)
import           NodeEditor.React.Model.Node.ExpressionNode (Visualization (Visualization), VisualizationMode (Focused, Zoomed),
                                                             getVisualization, nodeLoc, position, visualization, visualizationMode,
                                                             visualizer, visualizers)
import           NodeEditor.React.Model.NodeEditor          (visualizations)
import           NodeEditor.State.Action                    (Action (begin, continue, end, update),
                                                             VisualizationActive (VisualizationActive),
                                                             VisualizationDrag (VisualizationDrag), visualizationActiveAction,
                                                             visualizationActiveParentNodeLoc, visualizationDragAction)
import           NodeEditor.State.Global                    (State)
import           React.Flux                                 (MouseEvent)


instance Action (Command State) VisualizationActive where
    begin      = beginActionWithKey    visualizationActiveAction
    continue   = continueActionWithKey visualizationActiveAction
    update     = updateActionWithKey   visualizationActiveAction
    end action = do
        modifyExpressionNode (action ^. visualizationActiveParentNodeLoc) $
            visualization . _Just . visualizationMode .= def
        removeActionFromState visualizationActiveAction

focusVisualization :: NodeLoc -> Command State ()
focusVisualization nl = whenM (isJust . maybe def getVisualization <$> getExpressionNode nl) $ do
    modifyExpressionNode nl $ visualization . _Just . visualizationMode .= Focused
    begin $ VisualizationActive nl

closeVisualization :: VisualizationActive -> Command State ()
closeVisualization = end

selectVisualization :: NodeLoc -> VisualizerName -> Command State ()
selectVisualization nl visName = withJustM (getExpressionNode nl) $ \n ->
    withJust (Map.lookup visName $ n ^. visualizers) $ \visPath ->
        when (n ^? visualization . _Just . visualizer /= Just (visName, visPath)) $ do
            continue $ closeVisualization
            visBackup <- getVisualizationsBackupMap
            case Map.lookup nl visBackup of
                Just StreamStart -> do
                    uuid <- getUUID
                    modifyExpressionNode nl $ visualization ?= Visualization (Just uuid) (visName, visPath) def
                    liftIO $ do
                        registerVisualizerFrame uuid
                        notifyStreamRestart     uuid
                Just (Value v) -> do
                    uuid <- getUUID
                    modifyExpressionNode nl $ visualization ?= Visualization (Just uuid) (visName, visPath) def
                    liftIO $ do
                        registerVisualizerFrame uuid
                        sendVisualisationData   uuid v
                _ -> modifyExpressionNode nl $ visualization ?= Visualization def (visName, visPath) def

zoomVisualization :: Command State ()
zoomVisualization = do
    nodes <- getSelectedNodes
    case nodes of
        [n] -> when (isJust $ getVisualization n) $ do
            let nl = n ^. nodeLoc
            continue $ closeVisualization
            modifyExpressionNode nl $ visualization . _Just . visualizationMode .= Zoomed
            -- begin $ VisualizationActive nl
        _   -> return ()




-- instance Action (Command State) VisualizationDrag where
--     begin    = beginActionWithKey    visualizationDragAction
--     continue = continueActionWithKey visualizationDragAction
--     update   = updateActionWithKey   visualizationDragAction
--     end _    = removeActionFromState visualizationDragAction
--
-- pin :: NodeLoc -> Int -> Command State ()
-- pin nl visIx = do
--     mayNode <- getExpressionNode nl
--     withJust mayNode $ \node ->
--         modifyNodeEditor $
--             visualizations %= ((nl, visIx, node ^. position) :)
--
-- unpin :: NodeLoc -> Int -> Position -> Command State ()
-- unpin nl visIx pos =
--     modifyNodeEditor $ visualizations %= delete (nl, visIx, pos)
--
-- startDrag :: NodeLoc -> Int -> Position -> MouseEvent -> Command State ()
-- startDrag nl visIx pos evt = do
--     begin $ VisualizationDrag nl visIx pos
--     moveTo evt nl visIx pos
--
-- drag :: MouseEvent -> VisualizationDrag -> Command State ()
-- drag evt (VisualizationDrag nl visIx pos) = moveTo evt nl visIx pos
--
-- stopDrag :: MouseEvent -> VisualizationDrag ->  Command State ()
-- stopDrag evt (VisualizationDrag nl visIx pos) = do
--     moveTo evt nl visIx pos
--     removeActionFromState visualizationDragAction
--
-- moveTo :: MouseEvent -> NodeLoc -> Int -> Position -> Command State ()
-- moveTo evt nl visIx oldPos = do
--     pos <- workspacePosition evt
--     update $ VisualizationDrag nl visIx pos
--     modifyNodeEditor $ do
--         visualizations %= delete (nl, visIx, oldPos)
--         visualizations %= ((nl, visIx, pos) :)
