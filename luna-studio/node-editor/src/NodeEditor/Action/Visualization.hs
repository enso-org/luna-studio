{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Visualization where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Map                                   as Map
import           JS.Visualizers                             (notifyStreamRestart, registerVisualizerFrame, sendVisualizationData)
import           LunaStudio.Data.NodeLoc                    (NodeLoc)
import           LunaStudio.Data.NodeMeta                   (displayResult)
import           LunaStudio.Data.NodeValue                  (VisualizerName)
import           LunaStudio.Data.TypeRep                    (toConstructorRep)
import           NodeEditor.Action.Basic                    (selectNode, setNodeMeta)
import           NodeEditor.Action.State.Action             (beginActionWithKey, checkAction, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodeType, getNodeMeta, getNodeVisualizations, getSelectedNodes,
                                                             getVisualizationsBackupMap, modifyExpressionNode, modifyNodeEditor,
                                                             updateDefaultVisualizer, updatePreferedVisualizer)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (nodeLoc, visualizationsEnabled)
import           NodeEditor.React.Model.NodeEditor          (VisualizationBackup (StreamBackup, ValueBackup), nodeVisualizations)
import           NodeEditor.React.Model.Visualization       (IdleVisualization (IdleVisualization),
                                                             RunningVisualization (RunningVisualization), VisualizationId,
                                                             VisualizationMode (Focused, FullScreen, Preview),
                                                             VisualizationStatus (Outdated, Ready), idleVisualizations, idleVisualizer,
                                                             runningVisualizer, stopVisualizations, visualizationId, visualizationMode,
                                                             visualizationStatus, visualizations, visualizers)
import           NodeEditor.State.Action                    (Action (begin, continue, end, update),
                                                             VisualizationActive (VisualizationActive), visualizationActiveAction,
                                                             visualizationActiveNodeLoc, visualizationActiveSelectedMode,
                                                             visualizationActiveTriggeredByVis, visualizationActiveVisualizationId)
import           NodeEditor.State.Global                    (State)


instance Action (Command State) VisualizationActive where
    begin action = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        beginActionWithKey visualizationActiveAction action
        selectNode nl
        modifyNodeEditor $ nodeVisualizations . ix nl . visualizations . ix visId . visualizationMode .=
            action ^. visualizationActiveSelectedMode
    continue     = continueActionWithKey visualizationActiveAction
    update       = updateActionWithKey   visualizationActiveAction
    end action   = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        modifyNodeEditor $ nodeVisualizations . ix nl . visualizations . ix visId . visualizationMode .= def
        removeActionFromState visualizationActiveAction
        when (action ^. visualizationActiveTriggeredByVis) $ begin $ action & visualizationActiveSelectedMode   .~ Focused
                                                                            & visualizationActiveTriggeredByVis .~ False


focusVisualization :: NodeLoc -> VisualizationId -> Command State ()
focusVisualization nl visId = begin $ VisualizationActive nl visId Focused False

exitVisualizationMode :: VisualizationActive -> Command State ()
exitVisualizationMode = end

selectVisualizer :: NodeLoc -> VisualizationId -> VisualizerName -> Command State ()
selectVisualizer nl visId visName = withJustM (getNodeVisualizations nl) $ \nodeVis ->
    withJust ((,) <$> Map.lookup visId (nodeVis ^. visualizations) <*> Map.lookup visName (nodeVis ^. visualizers)) $ \(prevVis, visPath) -> do
        continue (end :: VisualizationActive -> Command State ())
        let visualizer' = (visName, visPath)
        updateDefaultVisualizer nl (Just visualizer') True
        when (prevVis ^. runningVisualizer /= visualizer') $ getVisualizationsBackupMap >>= \visBackup ->
            case Map.lookup nl visBackup of
                Just (StreamBackup backup) -> do
                    uuid <- getUUID
                    modifyNodeEditor $ do
                        nodeVisualizations . ix nl . visualizations . at (prevVis ^. visualizationId) .= def
                        nodeVisualizations . ix nl . visualizations . at uuid ?= RunningVisualization uuid def visualizer'
                    mayTpe <- getExpressionNodeType nl
                    withJust ((,) <$> mayTpe <*> maybe def toConstructorRep mayTpe) $ \(tpe, cRep) -> do
                        updatePreferedVisualizer tpe visualizer'
                        liftIO $ do
                            registerVisualizerFrame uuid
                            notifyStreamRestart     uuid cRep (reverse backup)
                Just (ValueBackup backup) -> do
                    uuid <- getUUID
                    modifyNodeEditor $ do
                        nodeVisualizations . ix nl . visualizations . at (prevVis ^. visualizationId) .= def
                        nodeVisualizations . ix nl . visualizations . at uuid ?= RunningVisualization uuid def visualizer'
                    mayTpe <- getExpressionNodeType nl
                    withJust ((,) <$> mayTpe <*> maybe def toConstructorRep mayTpe) $ \(tpe, cRep) -> do
                        updatePreferedVisualizer tpe visualizer'
                        liftIO $ do
                            registerVisualizerFrame uuid
                            sendVisualizationData   uuid cRep backup
                _ -> do
                    modifyNodeEditor $ do
                        nodeVisualizations . ix nl . visualizations . at (prevVis ^. visualizationId) .= def
                        nodeVisualizations . ix nl . idleVisualizations %= (IdleVisualization Ready visualizer' :)
                    withJustM (getExpressionNodeType nl) $ \tpe ->
                        updatePreferedVisualizer tpe visualizer'

handleZoomVisualization :: Command State ()
handleZoomVisualization = do
    mayMode <- view visualizationActiveSelectedMode `fmap2` checkAction visualizationActiveAction
    if mayMode == Just FullScreen
        then continue exitVisualizationMode
        else enterVisualizationMode FullScreen

exitPreviewMode :: VisualizationActive -> Command State ()
exitPreviewMode action = when (Preview == action ^. visualizationActiveSelectedMode) $
    exitVisualizationMode action

enterVisualizationMode :: VisualizationMode -> Command State ()
enterVisualizationMode visMode = do
    -- TODO: Allow change to any visualization
    visLoc <- getSelectedNodes >>= \case
        [n] -> let nl = n ^. nodeLoc in
            fmap (nl,) . maybe def (listToMaybe . Map.keys . view visualizations) <$> getNodeVisualizations nl
        _   -> return Nothing
    fromVis <- maybe False (\action -> action ^. visualizationActiveSelectedMode == Focused || action ^. visualizationActiveTriggeredByVis) <$> checkAction visualizationActiveAction
    withJust visLoc $ \(nl, visId) -> begin $ VisualizationActive nl visId visMode fromVis

toggleVisualizations :: NodeLoc -> Command State ()
toggleVisualizations nl = do
    modifyExpressionNode nl $ visualizationsEnabled %= not
    mayNodeMeta <- getNodeMeta nl
    withJust mayNodeMeta $ setNodeMeta . (nl,)
    stopVisualizationsForNode nl
    when (maybe False (view displayResult) mayNodeMeta) $ startReadyVisualizations nl

stopVisualizationsForNode :: NodeLoc -> Command State ()
stopVisualizationsForNode nl = modifyNodeEditor $ nodeVisualizations . ix nl %= stopVisualizations

startReadyVisualizations :: NodeLoc -> Command State ()
startReadyVisualizations nl = do
    mayVisBackup <- Map.lookup nl <$> getVisualizationsBackupMap
    mayNodeVis   <- getNodeVisualizations nl
    mayCRep      <- maybe def toConstructorRep <$> getExpressionNodeType nl
    withJust ((,,) <$> mayVisBackup <*> mayNodeVis <*> mayCRep) $ \(visBackup, nodeVis, cRep) -> case visBackup of
        StreamBackup backup -> do
            let activateWithStreamStart newNodeVis vis = if vis ^. visualizationStatus == Outdated then return $ newNodeVis & idleVisualizations %~ (vis:) else do
                    uuid <- getUUID
                    liftIO $ registerVisualizerFrame uuid >> notifyStreamRestart uuid cRep (reverse backup)
                    return $ newNodeVis & visualizations %~ Map.insert uuid (RunningVisualization uuid def $ vis ^. idleVisualizer)
            nVis <- foldlM activateWithStreamStart (nodeVis & idleVisualizations .~ def) $ nodeVis ^. idleVisualizations
            modifyNodeEditor $ nodeVisualizations . at nl ?= nVis
        ValueBackup backup -> do
            let activateWithValue newNodeVis vis = if vis ^. visualizationStatus == Outdated then return $ newNodeVis & idleVisualizations %~ (vis:) else do
                    uuid <- getUUID
                    liftIO $ registerVisualizerFrame uuid >> sendVisualizationData uuid cRep backup
                    return $ newNodeVis & visualizations %~ Map.insert uuid (RunningVisualization uuid def $ vis ^. idleVisualizer)
            nVis <- foldlM activateWithValue (nodeVis & idleVisualizations .~ def) $ nodeVis ^. idleVisualizations
            modifyNodeEditor $ nodeVisualizations . at nl ?= nVis

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
