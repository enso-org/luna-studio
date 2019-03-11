{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.NodeValue where

import           Common.Prelude

import qualified Data.Text                                  as Text
import qualified LunaStudio.Data.Error                      as LunaError
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.View.Style                as Style
import qualified React.Flux                                 as React

import NodeEditor.React.IsRef                     (IsRef)
import NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, Value (..))
import React.Flux                                 hiding (image_)


nodeValueName :: JSString
nodeValueName = "node-value"

nodeValue_ :: IsRef r => r -> ExpressionNode -> ReactElementM ViewEventHandler ()
nodeValue_ ref n = React.view nodeValue (ref, n) mempty

nodeValue :: IsRef r => ReactView (r, ExpressionNode)
nodeValue = React.defineView nodeValueName $ \(_, n) ->
    div_
        [ "key"       $= "shortValuePositioner"
        , "className" $= Style.prefixFromList ["node__short-value-positioner", "noselect",  "node-translate"]
        ] $ do
        div_
            [ "key"       $= "shortValue"
            , "className" $= Style.prefixFromList ["node__short-value", "noselect" ]
            , onDoubleClick $ \e _ -> [stopPropagation e]
            ] $ elemString $ strValue n

strValue :: ExpressionNode -> String
strValue n = case n ^. Node.value of
    ShortValue value -> Text.unpack value
    Error      msg   -> showError msg
    _ -> ""


showError :: LunaError.Error LunaError.NodeError -> String
showError = showErrorSep ""

showSourceLocation :: LunaError.SourceLocation -> String
showSourceLocation (LunaError.SourceLocation mod klass fun) =
    "    " <> convert mod <> maybe "" (("." <>) . convert) klass <> "." <> convert fun <> "\n"

showCompileErrorBacktrace :: LunaError.CompileErrorDetails -> String
showCompileErrorBacktrace (LunaError.CompileErrorDetails arisingFrom requiredBy) =
    "\n" <>
    (if null arisingFrom then "" else ("arising from:\n" <> concatMap showSourceLocation arisingFrom <> "\n")) <>
    (if null requiredBy  then "" else ("required by:\n" <> concatMap showSourceLocation requiredBy))

showErrorSep :: String -> LunaError.Error LunaError.NodeError -> String
showErrorSep sep err = case err of
    LunaError.Error (LunaError.CompileError details) msg -> "Compile error: " <> sep <> convert msg <> sep <> showCompileErrorBacktrace details
    LunaError.Error LunaError.RuntimeError msg -> "Runtime error: " <> sep <> convert msg
