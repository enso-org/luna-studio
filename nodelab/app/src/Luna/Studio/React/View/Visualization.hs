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
import qualified Data.Aeson                                     as Aeson
import           Data.List.Split                                (wordsBy)
import           Data.Position                                  (Position)
import           Data.Size                                      (Size (Size), Vector2 (Vector2))
import qualified Data.Text                                      as Text
import           React.Flux                                     hiding (image_)
import qualified React.Flux                                     as React

import           Empire.API.Data.DefaultValue                   (Value (..))
import qualified Empire.API.Data.DefaultValue                   as DefaultValue
import qualified Empire.API.Data.Error                          as LunaError
import           Empire.API.Data.TypeRep                        (TypeRep)
import           Empire.API.Graph.NodeResultUpdate              (NodeValue)
import qualified Empire.API.Graph.NodeResultUpdate              as NodeResult
import           Luna.Studio.Data.Matrix                        (translatePropertyValue2)
import qualified Luna.Studio.Event.UI                           as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Visualization          as Visualization
import           Luna.Studio.React.Model.App                    (App)
import           Luna.Studio.React.Model.DataFrame              (DataFrame)
import qualified Luna.Studio.React.Model.DataFrame              as DataFrame
import qualified Luna.Studio.React.Model.Image                  as Image
import           Luna.Studio.React.Model.Node                   (Node, NodeId)
import qualified Luna.Studio.React.Model.Node                   as Node
import           Luna.Studio.React.Model.NodeEditor             (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor             as NodeEditor
import           Luna.Studio.React.Store                        (Ref, dispatch)
import           Luna.Studio.React.View.Visualization.DataFrame (dataFrame_)
import           Luna.Studio.React.View.Visualization.Graphics  (graphics_)
import           Luna.Studio.React.View.Visualization.Image     (image_)



viewName :: JSString
viewName = "visualization"

pinnedVisualization_ :: Ref App -> NodeEditor -> (NodeId, Int, Position) -> ReactElementM ViewEventHandler ()
pinnedVisualization_ ref ne (nodeId, _, position) =
    withJust (ne ^. NodeEditor.nodes . at nodeId) $ \node ->
        withJust (node ^. Node.value) $
            visualization_ ref nodeId $ Just position

visualization_ :: Ref App -> NodeId -> Maybe Position -> NodeValue -> ReactElementM ViewEventHandler ()
visualization_ ref nodeId mayPos v = React.view visualization (ref, nodeId, mayPos, v) mempty

visualization :: ReactView (Ref App, NodeId, Maybe Position, NodeValue)
visualization = React.defineView viewName $ \(ref, nodeId, mayPos, nodeValue) ->
    div_ [ "className" $= "luna-noselect" ] $
        case nodeValue of
            NodeResult.Error msg          -> nodeError_ msg
            NodeResult.Value _ valueReprs -> nodeValues_ ref nodeId mayPos valueReprs

errorMessageWrapMargin :: Int
errorMessageWrapMargin = 30

errorLen :: Int
errorLen = 40

strValue :: Node -> String
strValue n = convert $ case n ^. Node.value of
    Nothing -> ""
    Just (NodeResult.Value value []) -> value
    Just (NodeResult.Value value _ ) -> value
    Just (NodeResult.Error msg     ) -> limitString errorLen (convert $ showError msg)

limitString :: Int -> Text -> Text
limitString limit str | Text.length str > limit64 = Text.take limit64 str <> "…"
                      | otherwise                 = str
                      where limit64 = fromIntegral limit

wrapLines :: Int -> String -> String
wrapLines limit str = unlines . reverse $ foldl f [] $ words str where
    f (a:as) e = let t = a ++ " " ++ e in if length t <= limit then t:as else e:a:as
    f []     e = [e]

showError :: LunaError.Error TypeRep -> String
showError = showErrorSep ""

showErrorSep :: String -> LunaError.Error TypeRep -> String
showErrorSep sep err = case err of
    LunaError.ImportError   name     -> "Cannot find symbol \"" <> name        <> "\""
    LunaError.NoMethodError name tpe -> "Cannot find method \"" <> name        <> "\" for type \"" <> toString tpe <> "\""
    LunaError.TypeError     t1   t2  -> "Cannot match type  \"" <> toString t1 <> "\" with \""     <> toString t2  <> "\""
    LunaError.RuntimeError  msg      -> "Runtime error: " <> sep <> msg

nodeError_ :: LunaError.Error TypeRep -> ReactElementM ViewEventHandler ()
nodeError_ err = do
    let message = wrapLines errorMessageWrapMargin $ showErrorSep "\n" err
    div_
        [ "key"       $= "error"
        , "className" $= "luna-vis luna-vis--error"
        ] $ elemString message

nodeValues_ :: Ref App -> NodeId -> Maybe Position -> [Value] -> ReactElementM ViewEventHandler ()
nodeValues_ ref nodeId mayPos = mapM_ (uncurry $ nodeValue_ ref nodeId mayPos) . zip [0..]

nodeValue_ :: Ref App -> NodeId -> Maybe Position -> Int -> Value -> ReactElementM ViewEventHandler ()
nodeValue_ ref nodeId mayPos visIx value = do
    let isPinned = isJust mayPos
        event = case mayPos of
            Just pos -> \n v -> Visualization.Unpin n v pos
            Nothing  -> Visualization.Pin
        translatedDiv_ = case mayPos of
            Just pos -> div_ [ "className" $= "luna-node-trans luna-noselect luna-node-root"
                             , "style" @= Aeson.object
                                [ "zIndex"    Aeson..= show (1000 :: Integer)
                                , "transform" Aeson..= translatePropertyValue2 pos ] ]
                         . div_ [ "className" $= "luna-node__visuals" ]
            Nothing -> div_
    translatedDiv_ $ do
        withJust mayPos $ \pos ->
            button_ [ onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.VisualizationEvent $ Visualization.MouseDown m nodeId visIx pos)] $
                elemString "move"
        button_ [ onClick $ \_ _ -> dispatch ref $ UI.VisualizationEvent $ event nodeId visIx ] $
            elemString $ if isPinned then "unpin" else "pin"
        case value of
            DataFrame    cols -> do
                let heads  = convert . fst <$> cols
                    cols'  = fmap DefaultValue.stringify . snd <$> cols
                    rows   = transpose cols'
                    widget = DataFrame.create heads rows
                dataFrame_ visIx widget
            BoolValue       v -> strDiv $ show v
            DoubleList      v -> dataFrame_ visIx $ listTable $ convert . show <$> v
            DoublePairList  v -> dataFrame_ visIx $ listTablePairs $ mapTuple (convert . show) <$> v
            DoubleValue     v -> strDiv $ show v
            Graphics       gr -> graphics_ visIx gr
            Image     url w h -> image_ visIx $ Image.create (Size (Vector2 w h)) $ convert url
            IntList         v -> dataFrame_ visIx $ listTable $ convert . show <$> v
            IntPairList     v -> dataFrame_ visIx $ listTablePairs $ mapTuple (convert . show) <$> v
            IntValue        v -> strDiv $ show v
            Lambda        str -> strDiv str
            RationalValue   v -> strDiv $ show v
            StringList      v -> dataFrame_ visIx $ listTable $ convert <$> v
            StringMaybeList v -> dataFrame_ visIx $ listTable $ convert . show <$> v
            StringStringMap v -> dataFrame_ visIx $ listTablePairs $ mapTuple convert <$> v
            StringValue   str -> strDiv str
            _ -> return ()
    where
      strDiv = div_ . elemString . normalize

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
