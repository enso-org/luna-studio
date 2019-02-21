module NodeEditor.Action.Visualization where

import Common.Prelude

import qualified Data.Map                             as Map
import qualified NodeEditor.React.Model.Searcher      as Searcher
import qualified NodeEditor.React.Model.Visualization as Vis

import Common.Action.Command                      (Command)
import JS.Visualizers                             (notifyStreamRestart,
                                                   registerVisualizerFrame,
                                                   sendVisualizationData)
import LunaStudio.Data.NodeLoc                    (NodeLoc)
import LunaStudio.Data.TypeRep                    (toConstructorRep)
import NodeEditor.Action.Basic                    (selectNode, setNodeMeta)
import NodeEditor.Action.State.Action             (beginActionWithKey,
                                                   checkAction,
                                                   checkIfActionPerfoming,
                                                   continueActionWithKey,
                                                   removeActionFromState,
                                                   updateActionWithKey)
import NodeEditor.Action.State.NodeEditor         (getExpressionNode,
                                                   getExpressionNodeType,
                                                   getNodeMeta,
                                                   getNodeVisualizations,
                                                   getSelectedNodes,
                                                   getVisualizationBackup,
                                                   getVisualizersForType,
                                                   modifyExpressionNode,
                                                   modifyNodeEditor,
                                                   modifySearcher,
                                                   setVisualizationData,
                                                   updateDefaultVisualizer,
                                                   updatePreferedVisualizer)
import NodeEditor.Action.UUID                     (getUUID)
import NodeEditor.React.Model.Node.ExpressionNode (nodeLoc,
                                                   visualizationsEnabled)
import NodeEditor.React.Model.NodeEditor          (nodeVisualizations)
import NodeEditor.React.Model.Visualization       (RunningVisualization (RunningVisualization),
                                                   VisualizationBackup (MessageBackup, StreamBackup, ValueBackup),
                                                   VisualizationId,
                                                   VisualizationMode (Focused, FullScreen, Preview),
                                                   VisualizationParent (Node, Searcher),
                                                   VisualizationStatus (Outdated),
                                                   Visualizer (Visualizer),
                                                   VisualizerId,
                                                   VisualizerProperties (VisualizerProperties),
                                                   awaitingDataMsg,
                                                   idleVisualizations,
                                                   idleVisualizerProperties,
                                                   noVisMsg, runningVisualizer,
                                                   selectedVisualizerId,
                                                   stopVisualizations,
                                                   visualizationId,
                                                   visualizationMode,
                                                   visualizationStatus,
                                                   visualizations,
                                                   visualizerProperties,
                                                   visualizers)
import NodeEditor.State.Action                    (Action (begin, continue, end, update),
                                                   DocVisualizationActive (DocVisualizationActive),
                                                   VisualizationActive (VisualizationActive),
                                                   docVisualizationActiveAction,
                                                   docVisualizationActiveSelectedMode,
                                                   docVisualizationActiveTriggeredByVis,
                                                   searcherAction,
                                                   visualizationActiveAction,
                                                   visualizationActiveNodeLoc,
                                                   visualizationActiveSelectedMode,
                                                   visualizationActiveTriggeredByVis,
                                                   visualizationActiveVisualizationId)
import NodeEditor.State.Global                    (State)


instance Action (Command State) VisualizationActive where
    begin action = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        beginActionWithKey visualizationActiveAction action
        selectNode nl
        modifyNodeEditor $ nodeVisualizations . ix nl . visualizations
            . ix visId . visualizationMode
                .= action ^. visualizationActiveSelectedMode
    continue     = continueActionWithKey visualizationActiveAction
    update       = updateActionWithKey   visualizationActiveAction
    end action   = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        modifyNodeEditor $ nodeVisualizations . ix nl . visualizations
            . ix visId . visualizationMode .= def
        removeActionFromState visualizationActiveAction
        when (action ^. visualizationActiveTriggeredByVis) $ begin $ action
            & visualizationActiveSelectedMode   .~ Focused
            & visualizationActiveTriggeredByVis .~ False

instance Action (Command State) DocVisualizationActive where
    begin action = do
        beginActionWithKey docVisualizationActiveAction action
        modifySearcher $ Searcher.documentationVisualization
            . visualizationMode .= action ^. docVisualizationActiveSelectedMode
    continue     = continueActionWithKey docVisualizationActiveAction
    update       = updateActionWithKey   docVisualizationActiveAction
    end action   = do
        modifySearcher $ Searcher.documentationVisualization
            . visualizationMode .= def
        removeActionFromState docVisualizationActiveAction
        when (action ^. docVisualizationActiveTriggeredByVis) $ begin $ action
            & docVisualizationActiveSelectedMode   .~ Focused
            & docVisualizationActiveTriggeredByVis .~ False


focusVisualization :: VisualizationParent -> VisualizationId -> Command State ()
focusVisualization (Node nl) visId
    = begin $ VisualizationActive nl visId Focused False
focusVisualization Searcher  _
    = begin $ DocVisualizationActive Focused False

exitVisualizationMode :: VisualizationActive -> Command State ()
exitVisualizationMode = end

exitDocVisualizationMode :: DocVisualizationActive -> Command State ()
exitDocVisualizationMode = end


selectVisualizer :: VisualizationParent -> VisualizationId -> VisualizerId
    -> Command State ()
selectVisualizer (Node nl) visId visualizerId
    = withJustM (getNodeVisualizations nl) $ \nodeVis ->
        withJust ((,)
            <$> Map.lookup visId (nodeVis ^. visualizations)
            <*> Map.lookup visualizerId (nodeVis ^. visualizers)
            ) $ \(prevVis, visPath) -> do
                continue (end :: VisualizationActive -> Command State ())
                let visualizer' = Visualizer visualizerId visPath
                updateDefaultVisualizer nl (Just visualizer') True
                unless (prevVis ^. visualizerProperties . runningVisualizer
                    == visualizer') $ getVisualizationBackup nl >>= \case
                        Just (StreamBackup backup) -> do
                            uuid <- getUUID
                            modifyNodeEditor $ do
                                nodeVisualizations . ix nl . visualizations
                                    . at (prevVis ^. visualizationId) .= def
                                nodeVisualizations . ix nl . visualizations
                                    . at uuid ?= (RunningVisualization
                                        uuid
                                        def
                                        $ VisualizerProperties
                                            visualizer'
                                            (Just $ visualizer'
                                                ^. Vis.visualizerId)
                                        )
                            mayTpe <- getExpressionNodeType nl
                            withJust ((,)
                                <$> mayTpe
                                <*> maybe def toConstructorRep mayTpe
                                ) $ \(tpe, cRep) -> do
                                    updatePreferedVisualizer tpe visualizer'
                                    liftIO $ do
                                        registerVisualizerFrame uuid
                                        notifyStreamRestart
                                            uuid cRep (reverse backup)
                        Just (ValueBackup backup) -> do
                            uuid <- getUUID
                            modifyNodeEditor $ do
                                nodeVisualizations . ix nl . visualizations
                                    . at (prevVis ^. visualizationId) .= def
                                nodeVisualizations . ix nl . visualizations
                                    . at uuid ?= (RunningVisualization
                                        uuid
                                        def
                                        $ VisualizerProperties
                                            visualizer'
                                            (Just $ visualizer'
                                                ^. Vis.visualizerId)
                                        )
                            mayTpe <- getExpressionNodeType nl
                            withJust ((,)
                                <$> mayTpe
                                <*> maybe def toConstructorRep mayTpe
                                ) $ \(tpe, cRep) -> do
                                    updatePreferedVisualizer tpe visualizer'
                                    liftIO $ do
                                        registerVisualizerFrame uuid
                                        sendVisualizationData   uuid cRep backup
                        _ -> do
                            modifyNodeEditor $ nodeVisualizations
                                . ix nl
                                . visualizations
                                . ix (prevVis ^. visualizationId)
                                . visualizerProperties
                                . selectedVisualizerId
                                ?= visualizerId
                            withJustM (getExpressionNodeType nl)
                                $ flip updatePreferedVisualizer visualizer'
selectVisualizer Searcher _ _ = $notImplemented


handleZoomVisualization :: Command State ()
handleZoomVisualization = do
    searcherActive <- checkIfActionPerfoming searcherAction
    let handleZoomVis = do
            mayMode <- view visualizationActiveSelectedMode
                `fmap2` checkAction visualizationActiveAction
            if mayMode == Just FullScreen
                then continue exitVisualizationMode
                else enterVisualizationMode FullScreen
        -- handleZoomDocVis = do
        --     mayDocMode <- view docVisualizationActiveSelectedMode
        --         `fmap2` checkAction docVisualizationActiveAction
        --     if mayDocMode == Just FullScreen
        --         then continue exitDocVisualizationMode
        --         else enterVisualizationMode FullScreen
    if searcherActive then return () else handleZoomVis

exitPreviewMode :: VisualizationActive -> Command State ()
exitPreviewMode action
    = when (Preview == action ^. visualizationActiveSelectedMode) $
        exitVisualizationMode action

exitDocPreviewMode :: DocVisualizationActive -> Command State ()
exitDocPreviewMode action
    = when (Preview == action ^. docVisualizationActiveSelectedMode) $
        exitDocVisualizationMode action

enterVisualizationMode :: VisualizationMode -> Command State ()
enterVisualizationMode visMode = do
    searcherActive <- checkIfActionPerfoming searcherAction
    -- let enterDocVisMode = do
    --         fromDocVis <- maybe False (\action -> action ^. docVisualizationActiveSelectedMode == Focused || action ^. docVisualizationActiveTriggeredByVis) <$> checkAction docVisualizationActiveAction
    --         begin $ DocVisualizationActive visMode fromDocVis
    let enterVisMode = do
            visLoc <- getSelectedNodes >>= \case
                [n] -> let nl = n ^. nodeLoc in
                    fmap (nl,) . maybe
                        def
                        (listToMaybe . Map.keys . view visualizations)
                        <$> getNodeVisualizations nl
                _   -> return Nothing
            fromVis <- maybe
                False
                (\action ->
                    action ^. visualizationActiveSelectedMode == Focused
                    || action ^. visualizationActiveTriggeredByVis
                )
                <$> checkAction visualizationActiveAction
            withJust visLoc $ \(nl, visId) -> begin
                $ VisualizationActive nl visId visMode fromVis
    if searcherActive then return () else enterVisMode

toggleVisualizations :: VisualizationParent -> Command State ()
toggleVisualizations (Node nl) = do
    modifyExpressionNode nl $ visualizationsEnabled %= not
    mayNodeMeta <- getNodeMeta nl
    withJust mayNodeMeta $ setNodeMeta nl
    stopVisualizationsForNode nl
    showVis <- maybe False (view visualizationsEnabled) <$> getExpressionNode nl
    when showVis $ startReadyVisualizations nl
toggleVisualizations Searcher = $notImplemented

stopVisualizationsForNode :: NodeLoc -> Command State ()
stopVisualizationsForNode nl
    = modifyNodeEditor $ nodeVisualizations . ix nl %= stopVisualizations

startReadyVisualizations :: NodeLoc -> Command State ()
startReadyVisualizations nl = do
    mayVisBackup <- getVisualizationBackup nl
    mayNodeVis   <- getNodeVisualizations  nl
    let activateWith newNodeVis vis =
            if vis ^. visualizationStatus == Outdated
                then return $ newNodeVis & idleVisualizations %~ (vis:)
                else do
                    uuid <- getUUID
                    liftIO $ registerVisualizerFrame uuid
                    return $ newNodeVis & visualizations
                        %~ Map.insert uuid (RunningVisualization
                            uuid
                            def
                            $ vis ^. idleVisualizerProperties)
        updateVis nodeVis (Just backup) = do
            nVis <- foldlM activateWith (nodeVis & idleVisualizations .~ def)
                $ nodeVis ^. idleVisualizations
            modifyNodeEditor $ nodeVisualizations . at nl ?= nVis
            setVisualizationData nl backup True
        updateVis nodeVis Nothing = do
            noVisForType <- maybe
                (return False)
                (fmap isNothing . getVisualizersForType)
                =<< getExpressionNodeType nl
            let msg = if noVisForType then noVisMsg else awaitingDataMsg
            updateVis nodeVis $ Just $ MessageBackup msg
    withJust mayNodeVis $ flip updateVis mayVisBackup

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
