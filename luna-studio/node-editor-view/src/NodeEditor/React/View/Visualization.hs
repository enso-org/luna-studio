module NodeEditor.React.View.Visualization where

import Common.Prelude
import React.Flux     hiding (image_)

import qualified Data.Aeson                           as Aeson
import qualified Data.Map                             as Map
import qualified JS.Mount                             as Mount
import qualified LunaStudio.Data.NodeLoc              as NodeLoc
import qualified NodeEditor.Event.UI                  as UI
import qualified NodeEditor.React.Event.Visualization as Visualization
import qualified NodeEditor.React.View.Style          as Style
import qualified React.Flux                           as React

import Data.Map                             (Map)
import NodeEditor.React.IsRef               (IsRef, dispatch)
import NodeEditor.React.Model.Constants     (lineHeight)
import NodeEditor.React.Model.Visualization (RunningVisualization,
                                             VisualizationId,
                                             VisualizationMode (Default, Focused, FullScreen, Preview),
                                             VisualizationParent (Node, Searcher),
                                             VisualizationProperties,
                                             Visualizer, VisualizerId,
                                             VisualizerPath,
                                             VisualizerType (ImportedVisualizer, InternalVisualizer, LunaVisualizer, ProjectVisualizer),
                                             Visualizers, externalVisualizers,
                                             internalVisualizers,
                                             librariesVisualizers,
                                             lunaVisualizers,
                                             projectVisualizers,
                                             runningVisualizer,
                                             selectedVisualizerId,
                                             visPropArgPortsNumber,
                                             visPropIsNodeExpanded,
                                             visPropNodeLoc,
                                             visPropVisualization,
                                             visPropVisualizers,
                                             visualizationId, visualizationMode,
                                             visualizerId, visualizerName,
                                             visualizerProperties,
                                             visualizerRelPath, visualizerType)


nodePrefix :: JSString
nodePrefix = Mount.prefix "node-"

visKey :: RunningVisualization -> JSString
visKey vis = Mount.prefix $ "visualization-" <> (fromString . show $ vis ^. visualizationId)

viewName, docViewName, iframeName,
    visMenuName, objNameVis, objNameShortVal :: JSString
viewName        = "visualization"
docViewName     = "doc-visualization"
iframeName      = "visualization-iframe"
visMenuName     = "visualizers"
objNameVis      = "node-vis"
objNameShortVal = "node-short-value"

getVisualizerPath :: VisualizerType -> Visualizers FilePath -> Maybe FilePath
getVisualizerPath tpe vp = case tpe of
    InternalVisualizer         -> Just $ vp ^. internalVisualizers
    LunaVisualizer             -> Just $ vp ^. lunaVisualizers
    ProjectVisualizer          -> vp ^. externalVisualizers . projectVisualizers
    ImportedVisualizer libName
        -> vp ^? externalVisualizers . librariesVisualizers . ix libName


nodeVisualization_ :: IsRef r => r -> Visualizers FilePath -> VisualizationProperties -> Bool -> ReactElementM ViewEventHandler ()
nodeVisualization_ ref visLibPaths visProp isNodeSelected = React.viewWithSKey nodeVisualization (visKey $ visProp ^. visPropVisualization) (ref, visLibPaths, visProp, isNodeSelected) mempty

nodeVisualization :: IsRef r => ReactView (r, Visualizers FilePath, VisualizationProperties, Bool)
nodeVisualization = React.defineView objNameVis $ \(ref, visLibPaths, visProp, isNodeSelected) -> do
    let nl             = visProp ^. visPropNodeLoc
        nid            = nl ^. NodeLoc.nodeId
        visualizers'   = visProp ^. visPropVisualizers
        vis            = visProp ^. visPropVisualization
        menuVisible    = elem (vis ^. visualizationMode) [Focused, Default]
        vmode          = vis ^. visualizationMode
        activeClass    = if vmode == Default then [] else [ "visualization--active" ]
        nSelectedClass = if isNodeSelected then [ "visualization--node-selected" ] else []
        classes        = if vmode == Preview || vmode == FullScreen then [ "visualization", "visualization--fullscreen", "noselect" ] else [ "visualization", "noselect" ]
        visShift       = show $ 4 + lineHeight * if visProp ^. visPropIsNodeExpanded then fromIntegral $ visProp ^. visPropArgPortsNumber else 0
        mayVisLibPath  = getVisualizerPath (vis ^. visualizerProperties . runningVisualizer . visualizerId . visualizerType) visLibPaths
    withJust mayVisLibPath $ \visLibPath -> div_
        [ "key"       $= visKey vis
        , "id"        $= (nodePrefix <> fromString (show nid))
        , "className" $= ("native-key-bindings " <> Style.prefixFromList (classes <> activeClass <> nSelectedClass ))
        , "style"     @= Aeson.object [ "transform" Aeson..= ("translate(-150px," <> visShift <> "rem)"::String) ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $
        div_
            [ "className" $= Style.prefix "node-translate"
            ] $ do
            visualization_   ref visLibPath (Node nl) vis True False
            visualizersMenu_ ref (Node nl) vis visualizers' menuVisible

docVisualization_ :: IsRef r => r -> Bool -> FilePath -> RunningVisualization -> ReactElementM ViewEventHandler ()
docVisualization_ ref docPresent visLibPath vis = React.viewWithSKey docVisualization (visKey vis) (ref, docPresent, visLibPath, vis) mempty

docVisualization :: IsRef r => ReactView (r, Bool, FilePath, RunningVisualization)
docVisualization = React.defineView docViewName $ \(ref, docPresent, visLibPath, vis) -> do
    let vmode        = vis ^. visualizationMode
        visibleClass = if docPresent then [] else ["hide"]
        activeClass  = if vmode == Default then [] else [ "visualization--active" ]
        fsModeClass  = if vmode == Preview || vmode == FullScreen then [ "visualization", "visualization--fullscreen", "noselect" ] else [ "visualization", "noselect" ]
        classes      = "searcher__doc" : visibleClass <> fsModeClass <> activeClass
    div_
        [ "key"       $= "doc"
        , "className" $= ("native-key-bindings " <> Style.prefixFromList classes)
        ] $ visualization_ ref visLibPath Searcher (vis & visualizationMode .~ Preview) docPresent True


visualizersMenu_ :: IsRef r => r -> VisualizationParent -> RunningVisualization -> Map VisualizerId VisualizerPath -> Bool -> ReactElementM ViewEventHandler ()
visualizersMenu_ ref visParent vis visMap visible = React.view visualizersMenu (ref, visParent, vis, visMap, visible) mempty

visualizersMenu :: IsRef r => ReactView (r, VisualizationParent, RunningVisualization, Map VisualizerId VisualizerPath, Bool)
visualizersMenu = React.defineView visMenuName $ \(ref, visParent, vis, visualizersMap, visible) -> do
    let selectedVisId = vis ^. visualizerProperties . selectedVisualizerId
        visId         = vis ^. visualizationId
    when (Map.size visualizersMap > 1 && Map.keys visualizersMap /= maybeToList selectedVisId) $ do
        let getVisualizerName visualizerId = case visualizerId ^. visualizerType of
                ProjectVisualizer
                    -> "project: " <> (visualizerId ^. visualizerName)
                ImportedVisualizer libName
                    -> libName <> ": " <> visualizerId ^. visualizerName
                _   -> visualizerId ^. visualizerName
            menuEntry :: VisualizerId -> ReactElementM ViewEventHandler ()
            menuEntry visualizerId = when (Just visualizerId /= selectedVisId) $
                li_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ Visualization.Event visParent $ Visualization.SelectVisualizer visId visualizerId ] . elemString . convert $ getVisualizerName visualizerId
        div_
            [ "className" $= Style.prefix (if visible then "dropdown" else "hide")
            ] $ do
                span_ $ elemString $ "▾"--convert actVisName
                ul_ [ "className" $= Style.prefix "dropdown__menu" ] $ mapM_ menuEntry $ Map.keys visualizersMap

visualization_ :: IsRef r => r -> FilePath -> VisualizationParent -> RunningVisualization -> Bool -> Bool -> ReactElementM ViewEventHandler ()
visualization_ ref visLibPath visParent vis isVisible noCover = React.view visualization (ref, visLibPath, visParent, vis, isVisible, noCover) mempty

visualization :: IsRef r => ReactView (r, FilePath, VisualizationParent, RunningVisualization, Bool, Bool)
visualization = React.defineView viewName $ \(ref, visLibPath, visParent, vis, isVisible, noCover) -> do
    let visId         = vis ^. visualizationId
        vmode         = vis ^. visualizationMode
        visualizer    = vis ^. visualizerProperties . runningVisualizer
        coverHandler  = if vmode == Default
            then [ onClick $ \_ _   -> dispatch ref $ UI.VisualizationEvent $ Visualization.Event visParent $ Visualization.Focus visId]
            else [ onWheel $ \e _ _ -> [stopPropagation e, preventDefault e] ]
    div_
        [ "className" $= Style.prefixFromList [ "noselect", "visualization-container" ]
        ] $ do
        unless noCover $
            div_ ([ "className" $= Style.prefix "visualization-cover" ] <> coverHandler) mempty
        visualizationIframe_ visLibPath visId visualizer isVisible

visualizationIframe_ :: FilePath -> VisualizationId -> Visualizer -> Bool -> ReactElementM ViewEventHandler ()
visualizationIframe_ visLibPath visId v isVisible = React.view visualizationIframe (visLibPath, visId, v, isVisible) mempty

visualizationIframe :: ReactView (FilePath, VisualizationId, Visualizer, Bool)
visualizationIframe = React.defineView iframeName $ \(visLibPath, visId, visualizer, isVisible) -> do
    iframe_
        [ "src"       $= (convert $ visLibPath </> (convert $ visualizer ^. visualizerRelPath))
        , "name"      $= (convert $ show visId)
        , "className" $= Style.prefix "visualization-iframe"
        , "height"    $= "300"
        , "width"     $= "300"
        ] mempty

-- pinnedVisualization_ :: IsRef r => r -> NodeEditor -> (NodeLoc, Int, Position) -> ReactElementM ViewEventHandler ()
-- pinnedVisualization_ ref ne (nl, _, position) =
--     withJust (NodeEditor.getExpressionNode nl ne >>= getVisualization) $ \(visId, visualizer, vmode) ->
--         visualization_ ref nl (Just position) visId visualizer vmode


-- iframe_
--     [ "srcDoc" $= ("<style>"
--                 <> "* { font:12px/16px Hasklig, monospace;color: #fff; padding:0; margin:0; border:none; }"
--                 <> "body { display:flex; justify-content:center; }"
--                 <> "table td { padding: 0 4px 2px }</style>"
--                 <> (convert $ strValue n) )
--     --, onMouseDown $ \_ _ -> traceShowMToStdout "NIE JEST NAJGORZEJ"
--     ] mempty
--
-- nodeValue_ :: IsRef r => r -> NodeLoc -> Maybe Position -> Int -> VisualizationValue -> ReactElementM ViewEventHandler ()
-- nodeValue_ ref nl mayPos visIx value = do
--     let isPinned = isJust mayPos
--         event = case mayPos of
--             Just pos -> \n v -> Visualization.Unpin n v pos
--             Nothing  -> Visualization.Pin
--         translatedDiv_ = if isJust mayPos
--             then div_ [ "className" $= Style.prefixFromList [ "node-trans", "noselect", "node-root" ]
--                       , "style"     @= Aeson.object [ "zIndex" Aeson..= show (1000 :: Integer) ]
--                       ] . div_ [ "className" $= Style.prefix "node__visuals" ]
--             else div_ [ "className" $= Style.prefixFromList ["noselect"] ]
--     translatedDiv_ $ do
--         withJust mayPos $ \pos ->
--             button_ [ onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.VisualizationEvent $ Visualization.MouseDown m nl visIx pos)
--                     , "className" $= "pin-button"
--                     , "key" $= "button1"
--                     ] $
--                 elemString "move"
--         button_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ event nl visIx
--                 , "key" $= "button2"
--                 , "className" $= "pin-button"
--                 ] $
--             elemString $ if isPinned then "unpin" else "pin"
--         case value of
--             JsonValue v -> fromJsonValue v
--             HtmlValue v -> strDiv v
--     where
--         strDiv = div_ [ "className" $= "visual" ] . elemString -- . normalize
--
-- fromJsonValue :: String -> ReactElementM ViewEventHandler ()
-- fromJsonValue value = case (Aeson.decode $ ByteString.pack value :: Maybe Aeson.Value) of
--     --Just (Aeson.Array  a) -> div_ [ "className" $= Style.prefix "table-scroll" ] $ table_ $ rows $ keyed $ Vector.toList a
--     Just (Aeson.Array  a) -> div_ [ "className" $= Style.prefix "table-scroll"
--                                   , onScroll    $ \e     -> [stopPropagation e]
--                                   , onWheel     $ \e _ _ -> [stopPropagation e]
--                                   ] $ table_ $ tbody_ $ rows $ keyed $ Vector.toList a
--     Just (Aeson.Object _) -> mempty
--     Just (Aeson.String a) -> div_ [ "className" $= Style.prefix "string-scroll"
--                                   , onScroll    $ \e     -> [stopPropagation e]
--                                   , onWheel     $ \e _ _ -> [stopPropagation e]
--                                   ] $ elemString $ convert a
--     Just (Aeson.Number _) -> mempty
--     Just (Aeson.Bool   _) -> mempty
--     Just (Aeson.Null    ) -> mempty
--     Nothing               -> mempty
--     where
--         rows []     = mempty
--         rows (x:xs) = do
--             fromJsonArray x
--             rows xs
--
-- fromJsonArray :: (Int, Aeson.Value) -> ReactElementM ViewEventHandler ()
-- fromJsonArray (k, val) = case val of
--     Aeson.Array  _ -> row "(Array)"
--     Aeson.Object _ -> row "(Object)"
--     Aeson.String a -> row $ convert a
--     Aeson.Number a -> row $ show $ coefficient a
--     Aeson.Bool   _ -> row "(Bool)"
--     Aeson.Null     -> row "(Null)"
--     where
--         cell = td_ . elemString
--         key  = cell $ show k
--         row x = tr_ $ do
--             key
--             cell x
