{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Visualization
    ( nodeShortValue_
    , nodeVisualizations_
    , visualization
    , visualization_
    , pinnedVisualization_
    , strValue
    , getVisualizators
    ) where

import           Common.Prelude
import qualified Data.Aeson                                 as Aeson
import qualified Data.ByteString.Lazy.Char8                 as ByteString
import qualified Data.HashMap.Lazy                          as Map
import           Data.Scientific                            (coefficient)
import           Data.Text                                  as Text
import qualified Data.Vector                                as Vector
import qualified LunaStudio.Data.Error                      as LunaError
import           LunaStudio.Data.NodeValue                  (NodeValue (..), VisualizationValue (..))
import           LunaStudio.Data.Position                   (Position)
import qualified NodeEditor.Event.UI                        as UI
import qualified NodeEditor.React.Event.Visualization       as Visualization
import           NodeEditor.React.Model.App                 (App, Visualizator, VisualizatorsMap)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, outPortAt)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Model.NodeEditor          (NodeEditor)
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import           NodeEditor.React.Store                     (Ref, dispatch)
import qualified NodeEditor.React.View.Style                as Style
import           React.Flux                                 hiding (image_)
import qualified React.Flux                                 as React

viewName, objNameVis, objNameShortVal :: JSString
viewName        = "visualization"
objNameVis      = "node-vis"
objNameShortVal = "node-short-value"

getVisualizators :: ExpressionNode -> VisualizatorsMap -> [Visualizator]
getVisualizators n vMap = fromMaybe [] $ maybe def (flip Map.lookup vMap) mayTpe where
    mayTpe = n ^? outPortAt [] . Port.valueType

nodeShortValue_ :: ExpressionNode -> ReactElementM ViewEventHandler ()
nodeShortValue_ model = React.viewWithSKey nodeShortValue objNameShortVal (model) mempty

nodeShortValue :: ReactView (ExpressionNode)
nodeShortValue = React.defineView objNameShortVal $ \(n) -> do
    div_
        [ "key"       $= "shortValue"
        , "className" $= Style.prefixFromList [ "node__short-value", "noselect" ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ elemString $ strValue n

nodeVisualizations_ :: Ref App -> ExpressionNode -> [Visualizator] -> ReactElementM ViewEventHandler ()
nodeVisualizations_ ref model v = React.viewWithSKey nodeVisualizations objNameVis (ref, model, v) mempty

nodeVisualizations :: ReactView (Ref App, ExpressionNode, [Visualizator])
nodeVisualizations = React.defineView objNameVis $ \(ref, n, visualizators) -> do
    let nodeLoc = n ^. Node.nodeLoc
    div_
        [ "key"       $= "visualizations"
        , "className" $= Style.prefixFromList [ "node__visualizations", "noselect" ]
        , onDoubleClick $ \e _ -> [stopPropagation e]
        ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeLoc def visualizators

pinnedVisualization_ :: Ref App -> NodeEditor -> VisualizatorsMap -> (NodeLoc, Int, Position) -> ReactElementM ViewEventHandler ()
pinnedVisualization_ ref ne vMap (nl, _, position) =
    withJust (NodeEditor.getExpressionNode nl ne) $ \node ->
        withJust (node ^. Node.value) $
            visualization_ ref nl (Just position) (getVisualizators node vMap)

visualization_ :: Ref App -> NodeLoc -> Maybe Position -> [Visualizator] -> NodeValue -> ReactElementM ViewEventHandler ()
visualization_ ref nl mayPos visualizators v = React.view visualization (ref, nl, mayPos, visualizators, v) mempty

visualization :: ReactView (Ref App, NodeLoc, Maybe Position, [Visualizator], NodeValue)
visualization = React.defineView viewName $ \(ref, nl, mayPos, visualizators, nodeValue) ->
    div_ [ "className" $= Style.prefixFromList [ "noselect" ] ] $
        case nodeValue of
            NodeValue _ valueReprs -> mapM_ (uncurry $ nodeValue_ ref nl mayPos) $ keyed valueReprs
            _                             -> mempty
-- iframe_
--     [ "srcDoc" $= ("<style>"
--                 <> "* { font:12px/16px Hasklig, monospace;color: #fff; padding:0; margin:0; border:none; }"
--                 <> "body { display:flex; justify-content:center; }"
--                 <> "table td { padding: 0 4px 2px }</style>"
--                 <> (convert $ strValue n) )
--     --, onMouseDown $ \_ _ -> traceShowMToStdout "NIE JEST NAJGORZEJ"
--     ] mempty

nodeValue_ :: Ref App -> NodeLoc -> Maybe Position -> Int -> VisualizationValue -> ReactElementM ViewEventHandler ()
nodeValue_ ref nl mayPos visIx value = do
    let isPinned = isJust mayPos
        event = case mayPos of
            Just pos -> \n v -> Visualization.Unpin n v pos
            Nothing  -> Visualization.Pin
        translatedDiv_ = if isJust mayPos
            then div_ [ "className" $= Style.prefixFromList [ "node-trans", "noselect", "node-root" ]
                      , "style"     @= Aeson.object [ "zIndex" Aeson..= show (1000 :: Integer) ]
                      ] . div_ [ "className" $= Style.prefix "node__visuals" ]
            else div_ [ "className" $= Style.prefixFromList ["noselect"] ]
    translatedDiv_ $ do
        withJust mayPos $ \pos ->
            button_ [ onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.VisualizationEvent $ Visualization.MouseDown m nl visIx pos)
                    , "className" $= "pin-button"
                    , "key" $= "button1"
                    ] $
                elemString "move"
        button_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ event nl visIx
                , "key" $= "button2"
                , "className" $= "pin-button"
                ] $
            elemString $ if isPinned then "unpin" else "pin"
        case value of
            JsonValue v -> fromJsonValue v
            HtmlValue v -> strDiv v
    where
        strDiv = div_ [ "className" $= "visual" ] . elemString -- . normalize

fromJsonValue :: String -> ReactElementM ViewEventHandler ()
fromJsonValue value = case (Aeson.decode $ ByteString.pack value :: Maybe Aeson.Value) of
    --Just (Aeson.Array  a) -> div_ [ "className" $= Style.prefix "table-scroll" ] $ table_ $ rows $ keyed $ Vector.toList a
    Just (Aeson.Array  a) -> div_ [ "className" $= Style.prefix "table-scroll"
                                  , onScroll    $ \e     -> [stopPropagation e]
                                  , onWheel     $ \e _ _ -> [stopPropagation e]
                                  ] $ table_ $ tbody_ $ rows $ keyed $ Vector.toList a
    Just (Aeson.Object _) -> mempty
    Just (Aeson.String a) -> div_ [ "className" $= Style.prefix "string-scroll"
                                  , onScroll    $ \e     -> [stopPropagation e]
                                  , onWheel     $ \e _ _ -> [stopPropagation e]
                                  ] $ elemString $ convert a
    Just (Aeson.Number _) -> mempty
    Just (Aeson.Bool   _) -> mempty
    Just (Aeson.Null    ) -> mempty
    Nothing               -> mempty
    where
        rows []     = mempty
        rows (x:xs) = do
            fromJsonArray x
            rows xs

fromJsonArray :: (Int, Aeson.Value) -> ReactElementM ViewEventHandler ()
fromJsonArray (k, val) = case val of
    Aeson.Array  _ -> row "(Array)"
    Aeson.Object _ -> row "(Object)"
    Aeson.String a -> row $ convert a
    Aeson.Number a -> row $ show $ coefficient a
    Aeson.Bool   _ -> row "(Bool)"
    Aeson.Null     -> row "(Null)"
    where
        cell = td_ . elemString
        key  = cell $ show k
        row x = tr_ $ do
            key
            cell x

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    Nothing -> ""
    Just (NodeValue value _ ) -> Text.unpack value
    Just (NodeError msg     ) -> showError msg --limitString errorLen (convert $ showError msg)

showError :: LunaError.Error -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error -> String
showErrorSep sep err = case err of
    LunaError.Error LunaError.CompileError msg -> "Compile error: " <> sep <> convert msg
    LunaError.Error LunaError.RuntimeError msg -> "Runtime error: " <> sep <> convert msg
