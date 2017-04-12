{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization
( nodeShortValue_
, nodeVisualizations_
, visualization
, visualization_
, pinnedVisualization_
, strValue
) where

import           Control.Arrow                                  ((***))
import qualified Data.Aeson                                     as Aeson
import           Data.List.Split                                (wordsBy)
import           Data.Position                                  (Position)
import qualified Data.Text                                      as Text
import           React.Flux                                     hiding (image_)
import qualified React.Flux                                     as React
import qualified Empire.API.Data.Error                          as LunaError
import           Empire.API.Data.PortDefault                    (Value (..))
import qualified Empire.API.Data.PortDefault                    as PortDefault
import           Empire.API.Data.TypeRep                        (TypeRep)
import           Empire.API.Graph.NodeResultUpdate              (NodeValue)
import qualified Empire.API.Graph.NodeResultUpdate              as NodeResult
import qualified Luna.Studio.Event.UI                           as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Visualization          as Visualization
import           Luna.Studio.React.Model.App                    (App)
import           Luna.Studio.React.Model.DataFrame              (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame              as DataFrame
import qualified Luna.Studio.React.Model.Image                  as Image
import           Luna.Studio.React.Model.Node.ExpressionNode    (ExpressionNode, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode    as Node
import           Luna.Studio.React.Model.NodeEditor             (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor             as NodeEditor
import           Luna.Studio.React.Store                        (Ref, dispatch)
import qualified Luna.Studio.React.View.Style                   as Style
import           Luna.Studio.React.View.Visualization.DataFrame (dataFrame_)
import           Luna.Studio.React.View.Visualization.Image     (image_)

viewName, objNameVis, objNameShortVal :: JSString
viewName        = "visualization"
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
        ] $ forM_ (n ^. Node.value) $ visualization_ ref nodeLoc def

pinnedVisualization_ :: Ref App -> NodeEditor -> (NodeLoc, Int, Position) -> ReactElementM ViewEventHandler ()
pinnedVisualization_ ref ne (nl, _, position) =
    withJust (NodeEditor.getExpressionNode nl ne) $ \node ->
        withJust (node ^. Node.value) $
            visualization_ ref nl $ Just position

visualization_ :: Ref App -> NodeLoc -> Maybe Position -> NodeValue -> ReactElementM ViewEventHandler ()
visualization_ ref nl mayPos v = React.view visualization (ref, nl, mayPos, v) mempty

visualization :: ReactView (Ref App, NodeLoc, Maybe Position, NodeValue)
visualization = React.defineView viewName $ \(ref, nl, mayPos, nodeValue) ->
    div_ [ "className" $= Style.prefixFromList [ "noselect" ] ] $
        case nodeValue of
            NodeResult.Value _ valueReprs -> mapM_ (uncurry $ nodeValue_ ref nl mayPos) $ keyed valueReprs
            _                             -> mempty
-- iframe_
--     [ "srcDoc" $= ("<style>"
--                 <> "* { font:12px/16px Hasklig, monospace;color: #fff; padding:0; margin:0; border:none; }"
--                 <> "body { display:flex; justify-content:center; }"
--                 <> "table td { padding: 0 4px 2px }</style>"
--                 <> (convert $ strValue n) )
--     --, onMouseDown $ \_ _ -> traceShowMToStdout "NIE JEST NAJGORZEJ"
--     ] mempty

nodeValue_ :: Ref App -> NodeLoc -> Maybe Position -> Int -> Value -> ReactElementM ViewEventHandler ()
nodeValue_ ref nl mayPos visIx value = do
    let isPinned = isJust mayPos
        event = case mayPos of
            Just pos -> \n v -> Visualization.Unpin n v pos
            Nothing  -> Visualization.Pin
        translatedDiv_ = case mayPos of
            Just pos -> div_ [ "className" $= Style.prefixFromList [ "node-trans", "noselect", "node-root" ]
                             , "style"     @= Aeson.object [ "zIndex" Aeson..= show (1000 :: Integer) ]
                             ] . div_ [ "className" $= Style.prefix "node__visuals" ]
            Nothing -> div_
    translatedDiv_ $ do
        withJust mayPos $ \pos ->
            button_ [ onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.VisualizationEvent $ Visualization.MouseDown m nl visIx pos)
                    , "key" $= "button1"
                    ] $
                elemString "move"
        button_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ event nl visIx
                , "key" $= "button2"
                ] $
            elemString $ if isPinned then "unpin" else "pin"
        case value of
            BoolValue   v -> strDiv $ show v
            DoubleValue v -> strDiv $ show v
            IntValue    v -> strDiv $ show v
            StringValue v -> strDiv v
            _ -> return ()
    where
        strDiv = div_ . elemString . normalize

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    Nothing -> ""
    Just (NodeResult.Value value []) -> Text.unpack value
    Just (NodeResult.Value value _ ) -> Text.unpack value
    Just (NodeResult.Error msg     ) -> showError msg --limitString errorLen (convert $ showError msg)

showError :: LunaError.Error TypeRep -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error TypeRep -> String
showErrorSep sep err = case err of
    LunaError.ImportError   name     -> "Cannot find symbol \"" <> name        <> "\""
    LunaError.NoMethodError name tpe -> "Cannot find method \"" <> name        <> "\" for type \"" <> toString tpe <> "\""
    LunaError.TypeError     t1   t2  -> "Cannot match type  \"" <> toString t1 <> "\" with \""     <> toString t2  <> "\""
    LunaError.RuntimeError  msg      -> "Runtime error: " <> sep <> msg

listTable :: [Text] -> DataFrame
listTable col = DataFrame.create ["Index", "Value"] rows where
    nats = [1..] :: [Integer]
    idxs = convert . show <$> take (length col) nats
    cols = [idxs, col]
    rows = transpose cols

mapTuple :: (b -> c) -> (b, b) -> (c, c)
mapTuple = join (***)

listTablePairs :: [(Text, Text)] -> DataFrame
listTablePairs rows = DataFrame.create ["fst", "snd"] $ (\(f,s) -> [f,s]) <$> rows

normalize :: String -> String
normalize = intercalate "<br />" . wordsBy (== '\n')
