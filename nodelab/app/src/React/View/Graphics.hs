{-# LANGUAGE OverloadedStrings #-}
module React.View.Graphics where

import qualified Data.Text.Lazy         as Text
import           React.Flux

import qualified Graphics.API           as GR
import qualified Object.Widget.Graphics as Graphics
import qualified Object.Widget.Label    as Label
import qualified Style.Layout           as Style
import qualified UI.Instances           ()
import           Utils.PreludePlus
import qualified Utils.Shader           as Shader
import           Utils.Vector           hiding (normalize)



graphics_ :: Int -> GR.Graphics -> ReactElementM ViewEventHandler ()
graphics_ visIx (GR.Graphics layers) = do
    let items  = fromLayers layers
        labels = createLabels =<< layers
        widget = Graphics.create Style.visualizationWidgetSize items labels
        createLabels (GR.Layer _ _ (GR.Labels labels)) = createLabel <$> labels
        createLabel  (GR.Label (GR.Point x' y') fontSize align text) = Graphics.Label (Vector2 x' y') fontSize (labelAlign align) $ Text.pack text
        labelAlign GR.Left   = Label.Left
        labelAlign GR.Center = Label.Center
        labelAlign GR.Right  = Label.Right
    text_ $ elemString $ fromString $ show widget


fromLayers :: [GR.Layer] -> [Graphics.Item]
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
