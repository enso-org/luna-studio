{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.NodeValue where

import           Common.Prelude
import qualified Data.Text                                  as Text
import qualified LunaStudio.Data.Error                      as LunaError
import qualified NodeEditor.Event.UI                        as UI
import qualified NodeEditor.React.Event.Node                as Node
import qualified NodeEditor.React.Event.Visualization       as Visualization
import           NodeEditor.React.IsRef                     (IsRef, dispatch)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, Value (..))
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.Model.Visualization       as Vis
import qualified NodeEditor.React.View.Style                as Style
import           React.Flux                                 hiding (image_)
import qualified React.Flux                                 as React


nodeValueName :: JSString
nodeValueName = "node-value"

nodeValue_ :: IsRef r => r -> ExpressionNode -> NodeLoc -> ReactElementM ViewEventHandler ()
nodeValue_ ref n nl = React.view nodeValue (ref, n, nl) mempty

nodeValue :: IsRef r => ReactView (r, ExpressionNode, NodeLoc)
nodeValue = React.defineView nodeValueName $ \(ref, n, nl) -> do
    let mayVisVisible = const (n ^. Node.visualizationsEnabled) <$> n ^. Node.defaultVisualizer
    div_
        [ "key"       $= "shortValuePositioner"
        , "className" $= Style.prefixFromList ["node__short-value-positioner", "noselect",  "node-translate"]
        ] $ do
        div_
            [ "key"         $= "shortValue"
            , "className"   $= Style.prefixFromList (["node__short-value", "noselect" ] <> (withJust mayVisVisible $ \isVisualization -> [ if isVisualization then "arrow-expanded" else "arrow-collapsed" ]))
            , onDoubleClick $ \e _ -> [stopPropagation e]
            , onClick       $ \_ _ -> dispatch ref . UI.VisualizationEvent $ Visualization.Event (Vis.Node nl) Visualization.ToggleVisualizations
            ] $ elemString $ strValue n

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    ShortValue value -> Text.unpack value
    Error      msg   -> showError msg
    _ -> ""

showError :: LunaError.Error LunaError.NodeError -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error LunaError.NodeError -> String
showErrorSep sep err = case err of
    LunaError.Error LunaError.CompileError msg -> "Compile error: " <> sep <> convert msg
    LunaError.Error LunaError.RuntimeError msg -> "Runtime error: " <> sep <> convert msg
