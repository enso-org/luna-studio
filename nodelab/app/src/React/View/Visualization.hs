{-# LANGUAGE OverloadedStrings #-}
module React.View.Visualization where

import           Control.Arrow                     ((***))
import           Data.List.Split                   (wordsBy)
import qualified Data.Text.Lazy                    as Text

import           Empire.API.Data.DefaultValue      (Value (..))
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import qualified Empire.API.Data.Error             as LunaError
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.API.Graph.NodeResultUpdate (NodeValue)
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult
import qualified Graphics.API                      as GR
import           Object.UITypes                    (WidgetId)
import           Object.Widget                     (CompositeWidget, DisplayObjectClass)
import           Object.Widget.DataFrame           (DataFrame)
import qualified Object.Widget.DataFrame           as DataFrame
import qualified Object.Widget.Graphics            as Graphics
import qualified Object.Widget.Label               as Label
import qualified Object.Widget.LongText            as LongText
import qualified Object.Widget.Plots.Image         as Image
import           React.Flux
import qualified React.Flux                        as React
import           React.Store.Node                  (Node)
import qualified React.Store.Node                  as Node
import           Reactive.Commands.Command         (Command)
import qualified Reactive.Commands.UIRegistry      as UICmd
import qualified Reactive.State.UIRegistry         as UIRegistry
import qualified Style.Layout                      as Style
import qualified Style.Node                        as Style
import qualified UI.Handlers.Node                  as Node
import qualified UI.Instances                      ()
import           Utils.PreludePlus
import qualified Utils.Shader                      as Shader
import           Utils.Vector hiding (normalize)



viewName :: JSString
viewName = "visualization"


visualization :: ReactView NodeValue
visualization = React.defineView viewName $ \case
    NodeResult.Error msg          -> text_ $ elemString "err visualization"
    NodeResult.Value _ valueReprs -> text_ $ elemString "val visualization"

visualization_ :: NodeValue -> ReactElementM ViewEventHandler ()
visualization_ v = React.view visualization v mempty


strValue :: Node -> String
strValue n = Text.unpack $ case n ^. Node.value of
    Nothing -> ""
    Just (NodeResult.Value value []) -> value
    Just (NodeResult.Value value valueReprs) -> value
        -- visualizeNodeValueReprs widgetId valueReprs --TODO[react]
    Just (NodeResult.Error msg) ->
        limitString errorLen (Text.pack $ showError msg)
        -- . (Model.isError .~ True)
        -- visualizeError widgetId msg --TODO[react]

errorLen :: Int
errorLen = 40

limitString :: Int -> Text -> Text
limitString limit str | Text.length str > limit64 = Text.take limit64 str <> "..."
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

visualizeError :: WidgetId -> LunaError.Error TypeRep -> Command UIRegistry.State ()
visualizeError wid err = do
    groupId <- Node.valueGroupId wid
    let message = Text.pack $ wrapLines Style.errorMessageWrapMargin $ showErrorSep "\n" err
        widget = LongText.create Style.errorMessageWidgetSize message LongText.Left LongText.Code
    UICmd.register_ groupId widget def

listTable :: [Text] -> DataFrame
listTable col = DataFrame.create Style.plotSize ["Index", "Value"] rows where
    nats = [1..] :: [Integer]
    idxs = Text.pack . show <$> take (length col) nats
    cols = [idxs, col]
    rows = transpose cols

mapTuple :: (b -> c) -> (b, b) -> (c, c)
mapTuple = join (***)

listTablePairs :: [(Text, Text)] -> DataFrame
listTablePairs rows = DataFrame.create Style.plotSize ["fst", "snd"] $ (\(f,s) -> [f,s]) <$> rows

visualize :: (Eq a, CompositeWidget a, DisplayObjectClass a) => Int -> WidgetId -> (WidgetId -> Command UIRegistry.State ()) -> (a -> a) -> Command UIRegistry.State ()
visualize visIx wid create update = do
    groupId <- Node.valueGroupId wid
    currentVisualizations <- UICmd.children groupId
    let currentVisualization = currentVisualizations ^? ix visIx
        _cleanup = do
            widgets <- UICmd.children groupId
            mapM_ UICmd.removeWidget widgets

    case currentVisualization of
        Nothing -> do
            -- cleanup
            -- performIO $ putStrLn "Not found"
            create groupId
        Just currentVis -> do
            status <- UICmd.tryUpdate currentVis update
            -- performIO $ putStrLn $ "update " <> (show status) <> " " <> (show visIx) <> " " <> (show currentVisualization) <> " " <> (show currentVisualizations)
            unless status $ do
                UICmd.removeWidget currentVis
                create groupId

visualizeNodeValueReprs :: WidgetId -> [Value] -> Command UIRegistry.State ()
visualizeNodeValueReprs wid values = do
    groupId <- Node.valueGroupId wid
    currentVisualizations <- UICmd.children groupId
    let visualizationsToRemove = drop (length values) currentVisualizations
    mapM_ UICmd.removeWidget visualizationsToRemove
    mapM_ (uncurry $ visualizeNodeValue wid) (zip [0..] values)

visualizeNodeValue :: WidgetId -> Int -> Value -> Command UIRegistry.State ()
visualizeNodeValue wid visIx (StringList v) = do
    let widget = listTable $ Text.pack <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (IntList v) = do
    let widget = listTable $ Text.pack . show <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (DoubleList v) = do
    let widget = listTable $ Text.pack . show <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (StringMaybeList v) = do
    let widget = listTable $ Text.pack . show <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (StringStringMap v) = do
    let widget = listTablePairs $ (mapTuple Text.pack) <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (IntPairList v) = do
    let widget = listTablePairs $ mapTuple (Text.pack . show) <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (DoublePairList v) = do
    let widget = listTablePairs $ mapTuple (Text.pack . show) <$> v
        create groupId = UICmd.registerIx_ visIx groupId widget def
        update         = DataFrame.rows .~ (widget ^. DataFrame.rows)
    visualize visIx wid create update

visualizeNodeValue wid visIx (Image url w h) = do
    let create groupId = do
            let widget = Image.create (Vector2 w h) $ Text.pack url
            UICmd.registerIx_ visIx groupId widget def
        update = (Image.size . x .~ w)
               . (Image.size . y .~ h)
               . (Image.image  .~ Text.pack url)
    visualize visIx wid create update

visualizeNodeValue wid visIx (StringValue str) = do
    let normalize = intercalate "<br />" . wordsBy (== '\n')
        create groupId = do
            let widget = LongText.create Style.visualizationWidgetSize (Text.pack $ normalize str) LongText.Left LongText.Text
            UICmd.registerIx_ visIx groupId widget def
        update = LongText.value .~ Text.pack (normalize str)
    visualize visIx wid create update

visualizeNodeValue wid visIx (Lambda str) = do
    let normalize = intercalate "<br />" . wordsBy (== '\n')
        create groupId = do
            let widget = LongText.create Style.visualizationWidgetSize (Text.pack $ normalize str) LongText.Left LongText.Code
            UICmd.registerIx_ visIx groupId widget def
        update = LongText.value .~ Text.pack (normalize str)
    visualize visIx wid create update

visualizeNodeValue wid visIx (DataFrame cols) = do
    let heads = Text.pack <$> fst <$> cols
        cols' = fmap DefaultValue.stringify <$> snd <$> cols
        rows = transpose cols'

    let create groupId = do
            let widget = DataFrame.create Style.dataFrameWidgetSize heads rows
            UICmd.registerIx_ visIx groupId widget def
        update      = (DataFrame.headers .~ heads)
                    . (DataFrame.rows    .~ rows )
    visualize visIx wid create update

visualizeNodeValue wid visIx (Graphics (GR.Graphics layers)) = do
    let items  = fromLayers layers
    -- let items  = createItem <$> layers
        labels = createLabels =<< layers
        create groupId = do
            let widget = Graphics.create Style.visualizationWidgetSize items labels
            UICmd.registerIx_ visIx groupId widget def
        update = (Graphics.items  .~ items )
               . (Graphics.labels .~ labels)
    visualize visIx wid create update
    where
        createLabels (GR.Layer _ _ (GR.Labels labels)) = createLabel <$> labels
        createLabel  (GR.Label (GR.Point x' y') fontSize align text) = Graphics.Label (Vector2 x' y') fontSize (labelAlign align) $ Text.pack text
        labelAlign GR.Left   = Label.Left
        labelAlign GR.Center = Label.Center
        labelAlign GR.Right  = Label.Right

visualizeNodeValue _ _ _ = return ()


fromLayers :: [GR.Layer] -> [Graphics.Item]
-- fromLayers [l1, l2, l3] = [fromLayer1 l1, fromLayer2 l2, fromLayer3 l3] -- fixes the sizes of axis and makes charts scaling improper
fromLayers layers = createItem <$> layers where
    createItem (GR.Layer geometry trans _) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
        Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.createShaderBox geometry
        boxes = createBoxes trans

createBoxes :: GR.Placement -> [Graphics.Box]
createBoxes (GR.Transformations transf) = createBoxFromTransf <$> transf
createBoxes (GR.Translations    transl) = createBoxFromTransl <$> transl

createBoxFromTransf :: GR.Transformation -> Graphics.Box
createBoxFromTransf (GR.Transformation _ _ dx dy _ _) = Graphics.Box (Vector2 dx dy)

createBoxFromTransl :: GR.Point -> Graphics.Box
createBoxFromTransl (GR.Point dx dy) = Graphics.Box (Vector2 dx dy)

-- fromLayer1 :: GR.Layer -> Graphics.Item
-- fromLayer1 (GR.Layer _ trans _) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
--     Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.ShaderBox "s1" (Shader.Location (Vector2 0.848 0.008) (Vector2 0.0 0.0))
--     boxes = createBoxes trans
--
-- fromLayer2 :: GR.Layer -> Graphics.Item
-- fromLayer2 (GR.Layer _ trans _) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
--     Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.ShaderBox "s2" (Shader.Location (Vector2 0.008 0.848) (Vector2 0.0 0.0))
--     boxes = createBoxes trans
--
-- fromLayer3 :: GR.Layer -> Graphics.Item
-- fromLayer3 (GR.Layer _ trans _) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
--     Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.ShaderBox "s3" (Shader.Location (Vector2 0.032 0.032) (Vector2 0.0 0.0))
--     boxes = createBoxes trans
