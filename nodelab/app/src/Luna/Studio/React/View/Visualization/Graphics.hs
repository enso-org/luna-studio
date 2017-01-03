{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.Graphics where

import qualified Data.Text.Lazy          as Text
import           React.Flux              hiding (label_)

import qualified Graphics.API            as GR
import qualified Luna.Studio.Data.Shader as Shader
import           Luna.Studio.Data.Vector hiding (normalize)
import           Luna.Studio.Prelude
import qualified Object.Widget.Graphics  as Graphics
import qualified Object.Widget.Graphics  as G
import qualified Object.Widget.Label     as Label
import qualified Style.Layout            as Style



graphics_ :: Int -> GR.Graphics -> ReactElementM ViewEventHandler ()
graphics_ visIx (GR.Graphics layers) = do
    let items  = fromLayers layers
        labels = createLabels =<< layers
        widget = Graphics.create Style.visualizationWidgetSize items labels
        createLabels (GR.Layer _ _ (GR.Labels l)) = createLabel <$> l
        createLabel  (GR.Label (GR.Point x' y') fontSize align text) = Graphics.Label (Position (Vector2 x' y')) fontSize (labelAlign align) $ Text.pack text
        labelAlign GR.Left   = Label.Left
        labelAlign GR.Center = Label.Center
        labelAlign GR.Right  = Label.Right
    div_ ["className" $= "vis vis--graph"] $ do
        div_ [ "className" $= "blur" ] mempty
        svg_ $
            g_ [ "key" $= fromString (show visIx)] $ do
                forM_ labels $ (label_ $ widget ^. G.size)
                forM_ items $ (item_ $ widget ^. G.size)

label_ :: Size -> G.Label -> ReactElementM ViewEventHandler ()
label_ size label = do
    text_
        [ "x" $= fromString (show $ (label ^. G.labelPosition . x) * (size ^. x))
        , "y" $= fromString (show $ (label ^. G.labelPosition . y) * (size ^. y))
        --TODO fontSize
        --TODO fontAlignment
        ] $
        elemString $ fromString $ Text.unpack $ label ^. G.text

item_ :: Size -> G.Item -> ReactElementM ViewEventHandler ()
item_ size item = do
    g_ mempty --TODO implement

fromLayers :: [GR.Layer] -> [Graphics.Item]
fromLayers layers = createItem <$> layers where
    createItem (GR.Layer geometry trans _) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
        Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.createShaderBox geometry
        boxes = createBoxes trans

createBoxes :: GR.Placement -> [Graphics.Box]
createBoxes (GR.Transformations transf) = createBoxFromTransf <$> transf
createBoxes (GR.Translations    transl) = createBoxFromTransl <$> transl

createBoxFromTransf :: GR.Transformation -> Graphics.Box
createBoxFromTransf (GR.Transformation _ _ dx dy _ _) = Graphics.Box (Position (Vector2 dx dy))

createBoxFromTransl :: GR.Point -> Graphics.Box
createBoxFromTransl (GR.Point dx dy) = Graphics.Box (Position (Vector2 dx dy))
