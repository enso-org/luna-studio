{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization
( visualization
, visualization_
, pinnedVisualization_
, strValue
)
where

import           Control.Arrow                                  ((***))
import           Data.List.Split                                (wordsBy)
import           Data.Position                                  (Position)
import qualified Data.Text                                      as Text
import           React.Flux                                     hiding (image_)
import qualified React.Flux                                     as React
import qualified Empire.API.Data.Error                          as LunaError
import           Empire.API.Data.PortDefault                    (Value (..))
import           Empire.API.Data.TypeRep                        (TypeRep)
import           Empire.API.Graph.NodeResultUpdate              (NodeValue)
import qualified Empire.API.Graph.NodeResultUpdate              as NodeResult
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App                    (App)
import           Luna.Studio.React.Model.DataFrame              (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame              as DataFrame
import           Luna.Studio.React.Model.Node.ExpressionNode    (ExpressionNode, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode    as Node
import           Luna.Studio.React.Model.NodeEditor             (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor             as NodeEditor
import           Luna.Studio.React.Store                        (Ref)
import qualified Luna.Studio.React.View.Style                   as Style
import           Luna.Studio.React.View.Visualization.DataFrame (dataFrame_)
import           Luna.Studio.React.View.Visualization.Image     (image_)



viewName :: JSString
viewName = "visualization"

pinnedVisualization_ :: Ref App -> NodeEditor -> (NodeLoc, Int, Position) -> ReactElementM ViewEventHandler ()
pinnedVisualization_ ref ne (nl, _, position) =
    withJust (NodeEditor.getExpressionNode nl ne) $ \node ->
        withJust (node ^. Node.value) $
            visualization_ ref nl $ Just position

visualization_ :: Ref App -> NodeLoc -> Maybe Position -> NodeValue -> ReactElementM ViewEventHandler ()
visualization_ ref nl mayPos v = React.view visualization (ref, nl, mayPos, v) mempty

visualization :: ReactView (Ref App, NodeLoc, Maybe Position, NodeValue)
visualization = React.defineView viewName $ \(ref, nl, mayPos, nodeValue) ->
    div_ [ "className" $= Style.prefix "noselect" ] $
        case nodeValue of
            NodeResult.Value _ valueReprs -> nodeValues_ ref nl mayPos valueReprs
            otherwise                     -> mempty

nodeValues_ :: Ref App -> NodeLoc -> Maybe Position -> [Value] -> ReactElementM ViewEventHandler ()
nodeValues_ ref nl mayPos = mapM_ (uncurry $ nodeValue_ ref nl mayPos) . keyed

nodeValue_ :: Ref App -> NodeLoc -> Maybe Position -> Int -> Value -> ReactElementM ViewEventHandler ()
nodeValue_ ref nl mayPos visIx value = elemString "To som nodeValue"

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
