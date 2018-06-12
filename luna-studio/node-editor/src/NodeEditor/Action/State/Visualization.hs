{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module NodeEditor.Action.State.Visualization where

import           Common.Prelude                             hiding (get)

import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.Map.Lazy                              as Map
import qualified JS.Visualizers                             as JS
import qualified NodeEditor.Action.Batch                    as Batch
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.NodeEditor          as NE
import qualified NodeEditor.React.Model.Visualization       as Visualization
import qualified NodeEditor.State.Global                    as Global

import           Common.Action.Command                      (Command)
import           Control.Arrow                              ((&&&))
import           Data.Map.Lazy                              (Map)
import           LunaStudio.Data.TypeRep                    (TypeRep (TStar), toConstructorRep)
import           LunaStudio.Data.Visualizer                 (applyType, fromJSInternalVisualizersMap, fromJSVisualizersMap)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, getExpressionNodeType, getNodeEditor, getNodeMeta,
                                                             modifyExpressionNode, modifyNodeEditor)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.NodeEditor          (VisualizersPaths (VisualizersPaths))
import           NodeEditor.React.Model.Visualization       (Content (Data, Error, Message), Data (Stream, Value), IframeId, Mode (Default, Hidden),
                                                             NodeVisualizations (NodeVisualizations), Visualization (Visualization),
                                                             VisualizationId, Visualizer (Visualizer), VisualizerId (VisualizerId),
                                                             VisualizerPath, VisualizerType (LunaVisualizer, ProjectVisualizer),
                                                             activeVisualizations, awaitingDataMsg, content, dataVisualizations, errorVisId,
                                                             errorVisualizations, errorVisualizationsEnabled, iframeId, isActive,
                                                             isErrorVisualization, mode, noVisMsg, placeholderVisId, selectedVisualizerId,
                                                             visualizationId, visualizations, visualizationsEnabled, visualizer,
                                                             visualizerId, visualizers, _Data, _Error, _Stream)
import           NodeEditor.State.Global                    (State, internalVisualizers, preferedVisualizers)


updateVisualizers :: Maybe FilePath -> Command State ()
updateVisualizers mayProjectVisPath = do
    internalVisPath <- liftIO $ JS.getInternalVisualizersLibraryPath
    lunaVisPath     <- liftIO $ JS.getLunaVisualizersLibraryPath
    modifyNodeEditor $ NE.visualizersLibPaths
        .= VisualizersPaths internalVisPath lunaVisPath mayProjectVisPath

    internalVisMap
        <- liftIO $ fromJSInternalVisualizersMap <$> JS.mkInternalVisualizersMap
    lunaVisMap <- liftIO $ Map.mapKeys
        (flip VisualizerId LunaVisualizer)
        . fromJSVisualizersMap <$> JS.mkLunaVisualizersMap
    projectVisMap  <- case mayProjectVisPath of
        Nothing -> return mempty
        Just fp -> liftIO $ Map.mapKeys
            (flip VisualizerId ProjectVisualizer)
            . fromJSVisualizersMap <$> JS.mkProjectVisualizersMap fp
    Global.visualizers         .= Map.union lunaVisMap projectVisMap
    Global.internalVisualizers .= internalVisMap

getPlaceholderVisualizer :: Command State (Maybe Visualizer)
getPlaceholderVisualizer
    = fmap (Visualizer placeholderVisId) . Map.lookup placeholderVisId
    <$> use internalVisualizers

getErrorVisualizer :: Command State (Maybe Visualizer)
getErrorVisualizer = fmap (Visualizer errorVisId) . Map.lookup errorVisId
    <$> use internalVisualizers

getNodeVisualizations :: NodeLoc -> Command State (Maybe NodeVisualizations)
getNodeVisualizations nl
    = view (NE.nodeVisualizations . at nl) <$> getNodeEditor

getPreferedVisualizer :: NodeLoc -> Command State (Maybe Visualizer)
getPreferedVisualizer nl = do
    let findPrefVis tpe = HashMap.lookup tpe <$> use preferedVisualizers
    mayPrefVisId <- maybe (pure Nothing) findPrefVis
        =<< getExpressionNodeType nl
    visualizers' <- fromMaybe mempty
        <$> (view visualizers `fmap2` getNodeVisualizations nl)
    let mayFirstVisInMap
            = fmap (uncurry Visualizer) . listToMaybe $ Map.toList visualizers'
        fromPrefVis visId = maybe
            mayFirstVisInMap
            (Just . Visualizer visId)
            $ Map.lookup visId visualizers'
    pure $ maybe mayFirstVisInMap fromPrefVis mayPrefVisId

currentInternalVisualizer :: NodeVisualizations
    -> Command State (Maybe Visualizer)
currentInternalVisualizer nv = case nv ^. content of
    Message {} -> getPlaceholderVisualizer
    Error   {} -> getErrorVisualizer
    _                        -> pure Nothing

adjust :: Visualization -> NodeLoc -> NodeVisualizations
    -> Command State (Visualization)
adjust vis nl nv = do
    let visId         = vis ^. visualizationId
        visEnabled    = nv ^. visualizationsEnabled
        errVisEnabled = nv ^. errorVisualizationsEnabled
        visContent    = nv ^. content
        visualizerId' = vis ^. visualizer . visualizerId
        hide v        = v & mode .~ Hidden
        setDataVisualizer v newVisualizer = v
            & visualizer           .~ newVisualizer
            & selectedVisualizerId .~ Just (newVisualizer ^. visualizerId)
    mayCurrentInternalVisualizer <- currentInternalVisualizer nv
    mayAdjustedVisualizer        <- if isJust mayCurrentInternalVisualizer
        then pure mayCurrentInternalVisualizer
        else case Map.lookup visualizerId' $ nv ^. visualizers of
            Just visPath -> pure . Just $ Visualizer visualizerId' visPath
            Nothing      -> getPreferedVisualizer nl
    pure $ if not (isActive visEnabled errVisEnabled visContent vis)
        || isErrorVisualization vis
            then vis
            else case mayCurrentInternalVisualizer of
                Nothing -> maybe
                    (hide vis)
                    (setDataVisualizer vis)
                    mayAdjustedVisualizer
                Just iv -> vis & visualizer .~ iv

--TODO: Rethink what to do when nv not present
insertVisualization :: NodeLoc -> Visualization -> Command State Visualization
insertVisualization nl vis = getNodeVisualizations nl >>= \case
    Nothing -> pure vis
    Just nv -> do
        adjustedVis <- adjust vis nl nv
        let visId = vis ^. visualizationId
            visEnabled    = nv ^. visualizationsEnabled
            errVisEnabled = nv ^. errorVisualizationsEnabled
            visContent    = nv ^. content
            mayPrevVis = Map.lookup visId $ nv ^. visualizations
            prevActive = maybe
                False
                (isActive visEnabled errVisEnabled visContent)
                mayPrevVis
            visualizerChanged = (view visualizer <$> mayPrevVis)
                /= Just (adjustedVis ^. visualizer)
            iframeChanged = maybe
                True
                ((adjustedVis ^. iframeId /=) . view iframeId)
                $ mayPrevVis

            prevModeDefault = (view mode <$> mayPrevVis) /= Just Default
            prevModeHidden  = (view mode <$> mayPrevVis) /= Just Hidden
            -- TODO: Remove layerChanged when iframe replaced
            layerChanged    = case adjustedVis ^. mode of
                Default -> (view mode <$> mayPrevVis) /= Just Default
                Hidden  -> False
                _       -> prevModeDefault || prevModeHidden
            needsRegister 
                = isActive visEnabled errVisEnabled visContent adjustedVis
                    && ( not prevActive
                        || visualizerChanged
                        || iframeChanged
                        || layerChanged )
        finalVis <- if not needsRegister then pure adjustedVis else do
            uuid <- getUUID
            JS.registerVisualizerFrame uuid
            sendContent nl [uuid]
            pure $ adjustedVis & iframeId .~ uuid
        modifyNodeEditor
            $ NE.nodeVisualizations . ix nl . visualizations . at visId
                ?= finalVis
        pure finalVis

constructWithId :: NodeLoc -> VisualizationId -> Mode -> Visualizer
    -> Maybe VisualizerId -> Command State (Visualization)
constructWithId nl visId mode' visualizer' maySelected = getUUID >>=
    \iframeId -> pure
        $ Visualization visId iframeId mode' visualizer' maySelected

construct :: NodeLoc -> Mode -> Visualizer -> Maybe VisualizerId
    -> Command State (Visualization)
construct nl mode' visualizer' maySelected = getUUID >>= \visId
    -> constructWithId nl visId mode' visualizer' maySelected

constructVisualizersMap :: NodeLoc
    -> Command State (Map VisualizerId VisualizerPath)
constructVisualizersMap nl
    = getExpressionNodeType nl >>= getVisualizersForType where
        getVisualizersForType Nothing    = pure mempty
        getVisualizersForType (Just tpe) = use Global.visualizers
            >>= applyType tpe


getContent :: NodeLoc -> Command State (Maybe Content)
getContent nl = view content `fmap2` getNodeVisualizations nl

setContent :: NodeLoc -> Content -> Command State ()
setContent nl visContent = withJustM (getNodeVisualizations nl) $ \nv -> do
    let prevC = nv ^. content
    unless (nv ^. content == visContent) $ do
        modifyNodeEditor $ NE.nodeVisualizations . ix nl . content .= visContent
        let visMap        = nv ^. visualizations
            visEnabled    = nv ^. visualizationsEnabled
            errVisEnabled = nv ^. errorVisualizationsEnabled
        forM_ visMap $ \vis -> do
            newVis <- insertVisualization nl vis
            let needsUpdate = isActive visEnabled errVisEnabled visContent vis
                    && vis ^. iframeId == newVis ^. iframeId
            when needsUpdate $ sendContent nl [newVis ^. iframeId]

sendContent :: NodeLoc -> [IframeId] -> Command State ()
sendContent nl iframeIds = withJustM (getContent nl) $ \case
    Data dc ->
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl)
            $ \cRep -> forM_ iframeIds
                $ \iframeId -> case dc of
                    Value  v -> JS.sendVisualizationData iframeId cRep v
                    Stream s -> JS.notifyStreamRestart iframeId cRep $ reverse s
    Message msg ->
        forM_ iframeIds $ flip JS.sendInternalData msg
    Error msg ->
        forM_ iframeIds $ flip JS.sendInternalData msg

appendStreamDataPoint :: NodeLoc -> Text -> Command State ()
appendStreamDataPoint nl val = withJustM (getNodeVisualizations nl) $ \nv -> do
    when (has (content . _Data . _Stream) nv) $ do
        modifyNodeEditor
            $ NE.nodeVisualizations . ix nl . content . _Data . _Stream
                %= (val :)
        let iframeIds = Map.elems $ view iframeId <$> nv ^. activeVisualizations
        forM_ iframeIds $ flip JS.sendStreamDatapoint val

addErrorVisualization :: NodeLoc -> Command State ()
addErrorVisualization nl = withJustM getErrorVisualizer $ \errVisualizer -> do
    errVis <- construct nl def errVisualizer Nothing
    void $ insertVisualization nl errVis

addDataVisualization :: NodeLoc -> Command State ()
addDataVisualization nl = do
    mayPrefVis    <- getPreferedVisualizer nl
    mayCurrentVis <- if isJust mayPrefVis
        then pure mayPrefVis
        else getPlaceholderVisualizer
    withJust mayCurrentVis $ \visualizer' -> do
        vis <- construct nl def visualizer' $ view visualizerId <$> mayPrefVis
        void $ insertVisualization nl vis

addNodeVisualizations :: NodeLoc -> Content -> Command State ()
addNodeVisualizations nl content = do
    (visEnabled, errVisEnabled) <- maybe
        (False, False)
        (view ExpressionNode.visEnabled &&& view ExpressionNode.errorVisEnabled)
        <$> getExpressionNode nl
    visMap <- constructVisualizersMap nl
    let nv = NodeVisualizations mempty visMap content visEnabled errVisEnabled
    modifyNodeEditor $ NE.nodeVisualizations . at nl ?= nv
    addErrorVisualization nl
    addDataVisualization nl

removeNodeVisualizations :: NodeLoc -> Command State ()
removeNodeVisualizations nl
    = modifyNodeEditor $ NE.nodeVisualizations . at nl .= def

updateVisualizations :: NodeLoc -> Command State ()
updateVisualizations nl = withJustM (getNodeVisualizations nl) $
    mapM_ (insertVisualization nl) . view visualizations

adjustToType :: NodeLoc -> Command State ()
adjustToType nl = do
    visMap        <- constructVisualizersMap nl
    hasType       <- isJust <$> getExpressionNodeType nl
    let msg = Message $ if hasType && Map.null visMap
            then noVisMsg
            else awaitingDataMsg
        updateNodeVis nv = nv
            & visualizers .~ visMap
            & content     .~ msg
    contentChanged <- (Just msg /=)
        <$> (view content `fmap2` getNodeVisualizations nl)
    modifyNodeEditor $ NE.nodeVisualizations . ix nl %= updateNodeVis
    mayPrefVis  <- view visualizerId `fmap2` getPreferedVisualizer nl
    prevVisList <- Map.elems . maybe mempty (view visualizations)
        <$> getNodeVisualizations nl
    let updateSelected Nothing = mayPrefVis
        updateSelected (Just prevSelected) = if Map.member prevSelected visMap
            then Just prevSelected
            else mayPrefVis
    iframesToUpdate <- fmap catMaybes . forM prevVisList $ \vis -> do
        newVis <- insertVisualization nl
            $ vis & selectedVisualizerId %~ updateSelected
        pure $ if (contentChanged && newVis ^. iframeId /= vis ^. iframeId)
            then Just $ newVis ^. iframeId
            else Nothing
    sendContent nl iframesToUpdate

getVisualization :: NodeLoc -> VisualizationId
    -> Command State (Maybe Visualization)
getVisualization nl visId = maybe
    Nothing
    (view (visualizations . at visId))
    <$> getNodeVisualizations nl

setVisualizationMode :: NodeLoc -> VisualizationId -> Mode -> Command State ()
setVisualizationMode nl visId m = withJustM (getVisualization nl visId)
    $ \vis -> void . insertVisualization nl $ vis & mode .~ m

getNodeVisualizers :: NodeLoc -> Command State (Map VisualizerId VisualizerPath)
getNodeVisualizers nl
    = maybe mempty (view visualizers) <$> getNodeVisualizations nl

getNodeVisualizer :: NodeLoc -> VisualizerId -> Command State (Maybe Visualizer)
getNodeVisualizer nl visId
    = fmap (Visualizer visId) . Map.lookup visId <$> getNodeVisualizers nl

setDefaultVisualizer :: NodeLoc -> Maybe VisualizerId -> Command State ()
setDefaultVisualizer nl mayNewVis = withJustM (getExpressionNode nl) $ \n -> do
    when (n ^. ExpressionNode.defaultVisualizer /= mayNewVis) $ do
        modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= mayNewVis
        withJust mayNewVis $ \newVis ->
            withJustM (getExpressionNodeType nl) $ \tpe ->
                preferedVisualizers . at tpe ?= newVis
        withJustM (getNodeMeta nl) $ \nm
            -> Batch.setNodesMeta $ Map.fromList [(nl, nm)]

selectVisualizer :: NodeLoc -> VisualizationId -> VisualizerId
    -> Command State ()
selectVisualizer nl visId newVisualizerId
    = withJustM (getNodeVisualizations nl) $ \nv ->
        withJust (Map.lookup newVisualizerId $ nv ^. visualizers)
            $ \visualizerPath -> do
                let visualizer' = Visualizer newVisualizerId visualizerPath
                setDefaultVisualizer nl $ Just newVisualizerId
                withJust (Map.lookup visId $ nv ^. visualizations) $ \vis -> do
                    void $ insertVisualization nl $ vis
                        & visualizer           .~ visualizer'
                        & selectedVisualizerId ?~ newVisualizerId

toggleVisualizations :: NodeLoc -> Command State ()
toggleVisualizations nl = withJustM (getNodeVisualizations nl) $ \nv -> do
    let visEnabled    = nv ^. visualizationsEnabled
        errVisEnabled = nv ^. errorVisualizationsEnabled
    toUpdate <- if has (content . _Error) nv
        then do
            modifyNodeEditor
                $ NE.nodeVisualizations . ix nl . errorVisualizationsEnabled
                    %= not
            pure . Map.elems $ nv ^. errorVisualizations
        else do
            modifyNodeEditor
                $ NE.nodeVisualizations . ix nl . visualizationsEnabled
                    %= not
            pure . Map.elems $ nv ^. dataVisualizations
    forM_ toUpdate $ \vis -> do
        uuid <- getUUID
        insertVisualization nl $ vis & iframeId .~ uuid
