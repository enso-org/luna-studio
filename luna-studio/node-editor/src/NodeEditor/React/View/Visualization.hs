{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Visualization
    ( nodeShortValue_
    , nodeVisualizations_
    , visualization
    , visualization_
    , pinnedVisualization_
    , strValue
    ) where

import           Common.Prelude
import qualified Data.Aeson                                 as Aeson
import qualified Data.ByteString.Lazy.Char8                 as ByteString
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Scientific                            (coefficient)
import qualified Data.Text                                  as Text
import qualified Data.Vector                                as Vector
import qualified LunaStudio.Data.Error                      as LunaError
import           LunaStudio.Data.NodeValue                  (VisualizationId, VisualizationValue (..), Visualizer, VisualizerName,
                                                             VisualizerPath)
import           LunaStudio.Data.Position                   (Position)
import qualified NodeEditor.Event.UI                        as UI
import qualified NodeEditor.React.Event.Visualization       as Visualization
import           NodeEditor.React.Model.App                 (App)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, IsVisualizationActive, NodeLoc, Value (..), getVisualization)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Model.NodeEditor          (NodeEditor)
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import           NodeEditor.React.Store                     (Ref, dispatch)
import qualified NodeEditor.React.View.Style                as Style
import           React.Flux                                 hiding (image_)
import qualified React.Flux                                 as React


viewName, objNameVis, objNameShortVal :: JSString
viewName        = "visualization"
iframeName      = "visualization-iframe"
visMenuName     = "visualizers"
objNameVis      = "node-vis"
objNameShortVal = "node-short-value"

nodeShortValue_ :: ExpressionNode -> ReactElementM ViewEventHandler ()
nodeShortValue_ model = React.viewWithSKey nodeShortValue objNameShortVal (model) mempty

nodeShortValue :: ReactView (ExpressionNode)
nodeShortValue = React.defineView objNameShortVal $ \(n) -> do
    div_
        [ "key"       $= "shortValue"
        , "className" $= Style.prefixFromList [ "node__short-value", "noselect" ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ elemString $ strValue n

nodeVisualizations_ :: Ref App -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeVisualizations_ ref model = React.viewWithSKey nodeVisualizations objNameVis (ref, model) mempty

nodeVisualizations :: ReactView (Ref App, ExpressionNode)
nodeVisualizations = React.defineView objNameVis $ \(ref, n) -> do
    let nodeLoc = n ^. Node.nodeLoc
    div_
        [ "key"       $= "visualizations"
        , "className" $= Style.prefixFromList [ "node__visualizations", "noselect" ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ withJust (getVisualization n) $ \(visId, visualizer, active) -> do
            visualizersMenu_ ref nodeLoc (fst visualizer) $ n ^. Node.visualizers
            visualization_ ref nodeLoc def visId visualizer active


visualizersMenu_ :: Ref App -> NodeLoc -> VisualizerName -> Map VisualizerName VisualizerPath -> ReactElementM ViewEventHandler ()
visualizersMenu_ ref nl actVisName visMap = React.view visualizersMenu (ref, nl, actVisName, visMap) mempty

visualizersMenu :: ReactView (Ref App, NodeLoc, VisualizerName, Map VisualizerName VisualizerPath)
visualizersMenu = React.defineView visMenuName $ \(ref, nl, actVisName, visualizersMap) ->
    when (Map.size visualizersMap > 1 || (Map.size visualizersMap == 1 && Map.notMember actVisName visualizersMap)) $ do
        let menuEntry :: Text -> ReactElementM ViewEventHandler ()
            menuEntry name = when (name /= actVisName) $
                li_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ Visualization.SelectVisualization nl name ] $ elemString $ convert name
        div_ [ "className" $= Style.prefix "luna-dropdown" ] $ do
            span_ $ elemString $ convert actVisName
            ul_ [ "className" $= Style.prefix "luna-dropdown__menu" ] $ mapM_ menuEntry $ Map.keys visualizersMap


visualization_ :: Ref App -> NodeLoc -> Maybe Position -> VisualizationId -> Visualizer -> IsVisualizationActive -> ReactElementM ViewEventHandler ()
visualization_ ref nl mayPos visId v active = React.view visualization (ref, nl, mayPos, visId, v, active) mempty

visualization :: ReactView (Ref App, NodeLoc, Maybe Position, VisualizationId, Visualizer, IsVisualizationActive)
visualization = React.defineView viewName $ \(ref, nl, mayPos, visId, visualizer, active) -> do
    let coverClass = if active then "visualization-iframe-cover-hidden" else "visualization-iframe-cover"
    div_ [ "className" $= Style.prefixFromList [ "noselect", "visualization-container" ] ] $ do
        div_ [ "className" $= Style.prefix coverClass
             , onClick     $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ Visualization.Activate nl
             ] mempty
        div_ [ "className" $= Style.prefix "visualization-iframe-container"
             ] $ visualizationIframe_ ref visId visualizer
    -- mapM_ (uncurry $ nodeValue_ ref nl mayPos) $ keyed vData

visualizationIframe_ :: Ref App -> VisualizationId -> Visualizer -> ReactElementM ViewEventHandler ()
visualizationIframe_ ref visId v = React.view visualizationIframe (ref, visId, v) mempty

visualizationIframe :: ReactView (Ref App, VisualizationId, Visualizer)
visualizationIframe = React.defineView iframeName $ \(ref, visId, visualizer) ->
    iframe_ [ "src"   $= (convert $ snd visualizer)
            , "name"  $= (convert $ show visId)
            , "width"  $= "300"
            , "height" $= "300"
            ] mempty

pinnedVisualization_ :: Ref App -> NodeEditor -> (NodeLoc, Int, Position) -> ReactElementM ViewEventHandler ()
pinnedVisualization_ ref ne (nl, _, position) =
    withJust (NodeEditor.getExpressionNode nl ne >>= getVisualization) $ \(visId, visualizer, isActive) ->
        visualization_ ref nl (Just position) visId visualizer isActive


-- iframe_
--     [ "srcDoc" $= ("<style>"
--                 <> "* { font:12px/16px Hasklig, monospace;color: #fff; padding:0; margin:0; border:none; }"
--                 <> "body { display:flex; justify-content:center; }"
--                 <> "table td { padding: 0 4px 2px }</style>"
--                 <> (convert $ strValue n) )
--     --, onMouseDown $ \_ _ -> traceShowMToStdout "NIE JEST NAJGORZEJ"
--     ] mempty
--
-- nodeValue_ :: Ref App -> NodeLoc -> Maybe Position -> Int -> VisualizationValue -> ReactElementM ViewEventHandler ()
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

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    Nothing -> ""
    Just (ShortValue value) -> Text.unpack value
    Just (Error      msg  ) -> showError msg --limitString errorLen (convert $ showError msg)

showError :: LunaError.Error -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error -> String
showErrorSep sep err = case err of
    LunaError.Error LunaError.CompileError msg -> "Compile error: " <> sep <> convert msg
    LunaError.Error LunaError.RuntimeError msg -> "Runtime error: " <> sep <> convert msg
