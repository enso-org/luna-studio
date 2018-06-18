{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Visualization where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Map                                   as Map
import           JS.Visualizers                             (notifyStreamRestart, registerVisualizerFrame, sendVisualizationData)
import           LunaStudio.Data.TypeRep                    (toConstructorRep)
import           NodeEditor.Action.Basic                    (selectNode, setNodeMeta)
import           NodeEditor.Action.State.Action             (beginActionWithKey, checkAction, checkIfActionPerfoming, continueActionWithKey,
                                                             removeActionFromState, updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodeType, getNodeMeta, getSelectedNodes,
                                                             modifyExpressionNode, modifyNodeEditor, modifySearcher)
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import qualified NodeEditor.Action.State.Visualization      as Visualization
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (nodeLoc, visualizationsEnabled)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.React.Model.NodeEditor          (nodeVisualizations)
import qualified NodeEditor.React.Model.Searcher            as Searcher
import           NodeEditor.React.Model.Visualization       (Mode (Focused, FullScreen, Preview), Parent (Node, Searcher),
                                                             Visualization (Visualization), VisualizationId, Visualizer (Visualizer),
                                                             VisualizerId, mode, selectedVisualizerId, visualizationId, visualizations,
                                                             visualizer, visualizers)
import qualified NodeEditor.React.Model.Visualization       as Vis
import           NodeEditor.State.Action                    (Action (begin, continue, end, update),
                                                             DocVisualizationActive (DocVisualizationActive),
                                                             VisualizationActive (VisualizationActive), docVisualizationActiveAction,
                                                             docVisualizationActiveSelectedMode, docVisualizationActiveTriggeredByVis,
                                                             searcherAction, visualizationActiveAction, visualizationActiveNodeLoc,
                                                             visualizationActiveSelectedMode, visualizationActiveTriggeredByVis,
                                                             visualizationActiveVisualizationId)
import           NodeEditor.State.Global                    (State)


instance Action (Command State) VisualizationActive where
    begin action = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        beginActionWithKey visualizationActiveAction action
        selectNode nl
        Visualization.setVisualizationMode nl visId
            $ action ^. visualizationActiveSelectedMode
    continue     = continueActionWithKey visualizationActiveAction
    update       = updateActionWithKey   visualizationActiveAction
    end action   = do
        let nl    = action ^. visualizationActiveNodeLoc
            visId = action ^. visualizationActiveVisualizationId
        Visualization.setVisualizationMode nl visId def
        removeActionFromState visualizationActiveAction
        when (action ^. visualizationActiveTriggeredByVis) $ begin $ action
            & visualizationActiveSelectedMode   .~ Focused
            & visualizationActiveTriggeredByVis .~ False

instance Action (Command State) DocVisualizationActive where
    begin action = do
        beginActionWithKey docVisualizationActiveAction action
        modifySearcher $ Searcher.mode . Searcher._Node . _2
            . Searcher.docVisInfo . _Just . mode
                .= action ^. docVisualizationActiveSelectedMode
    continue     = continueActionWithKey docVisualizationActiveAction
    update       = updateActionWithKey   docVisualizationActiveAction
    end action   = do
        modifySearcher $ Searcher.mode . Searcher._Node . _2
            . Searcher.docVisInfo . _Just . mode .= def
        removeActionFromState docVisualizationActiveAction
        when (action ^. docVisualizationActiveTriggeredByVis) $ begin $ action
            & docVisualizationActiveSelectedMode   .~ Focused
            & docVisualizationActiveTriggeredByVis .~ False

exitAnyVisualizationMode :: Command State ()
exitAnyVisualizationMode
    =  continue exitDocVisualizationMode
    >> continue exitVisualizationMode where
        exitDocVisualizationMode :: DocVisualizationActive -> Command State ()
        exitDocVisualizationMode = end
        exitVisualizationMode :: VisualizationActive -> Command State ()
        exitVisualizationMode = end

focusVisualization :: Parent -> VisualizationId -> Command State ()
focusVisualization (Node nl) visId
    = begin $ VisualizationActive nl visId Focused False
focusVisualization Searcher  _
    = begin $ DocVisualizationActive Focused False

handleZoomVisualization :: Command State ()
handleZoomVisualization = do
    mayMode <- view visualizationActiveSelectedMode
        `fmap2` checkAction visualizationActiveAction
    enterVisualizationMode $ if mayMode == Just FullScreen
        then Focused
        else FullScreen

closePreview :: Command State ()
closePreview = continue _closePreview where
    _closePreview action = when
        (action ^. visualizationActiveSelectedMode == Preview)
        $ enterVisualizationMode Focused

openPreview :: Command State ()
openPreview = checkAction visualizationActiveAction >>= \mayAction -> do
    unless ((view visualizationActiveSelectedMode <$> mayAction)
        == Just FullScreen)
            $ enterVisualizationMode Preview

enterVisualizationMode :: Mode -> Command State ()
enterVisualizationMode visMode = _enterVisualizationMode where
    _enterVisualizationMode :: Command State ()
    _enterVisualizationMode = do
        mayNl    <- getNodeLoc
        mayVisId <- maybe (pure Nothing) getVisId mayNl
        withJust ((,) <$> mayNl <*> mayVisId) $ uncurry enterVisMode
    getNodeLoc :: Command State (Maybe ExpressionNode.NodeLoc)
    getNodeLoc = do
        let _getNodeLoc [n] = Just $ n ^. nodeLoc
            _getNodeLoc _   = Nothing
        _getNodeLoc <$> getSelectedNodes
    getVisId :: ExpressionNode.NodeLoc -> Command State (Maybe VisualizationId)
    getVisId nl = do
        let _getVisId nv = do
                let visMap = nv ^. Vis.activeVisualizations
                if Map.size visMap /= 1
                    then Nothing
                    else listToMaybe $ Map.keys visMap
        fmap join $ _getVisId `fmap2` Visualization.getNodeVisualizations nl
    enterVisMode :: ExpressionNode.NodeLoc -> VisualizationId -> Command State ()
    enterVisMode nl visId = unlessM (checkIfActionPerfoming searcherAction) $ do
        let action = VisualizationActive nl visId visMode False
        mayPrevVisAction <- checkAction visualizationActiveAction
        when (Just action /= mayPrevVisAction) $ begin action

selectVisualizer :: Parent -> VisualizationId -> VisualizerId
    -> Command State ()
selectVisualizer (Node nl) visId visualizerId = do
    continue (end :: VisualizationActive -> Command State ())
    Visualization.selectVisualizer nl visId visualizerId
selectVisualizer Searcher _ _ = $notImplemented


toggleVisualizations :: Parent -> Command State ()
toggleVisualizations (Node nl) = do
    modifyExpressionNode nl $ do
        hasError <- ExpressionNode.returnsError <$> get
        if hasError
            then ExpressionNode.errorVisEnabled %= not
            else ExpressionNode.visEnabled      %= not
    withJustM (getNodeMeta nl) $ setNodeMeta nl
    Visualization.toggleVisualizations nl
toggleVisualizations Searcher = $notImplemented
