{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeEditor where

import qualified Data.Aeson                            as Aeson
import qualified Data.HashMap.Strict                   as HashMap
import           React.Flux
import qualified React.Flux                            as React
import           React.Flux.Internal                   (el)

import qualified Event.UI                              as UI
import qualified Luna.Studio.Data.CoordsTransformation as CoordsTransformation
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude                   hiding (transform)
import qualified Luna.Studio.React.Event.NodeEditor    as NE
import           Luna.Studio.React.Model.NodeEditor    (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.React.Store               (Ref, dispatch, dt)
import           Luna.Studio.React.View.Connection     (connection_, currentConnection_)
import           Luna.Studio.React.View.Global
import           Luna.Studio.React.View.Node           (node_)
import           Luna.Studio.React.View.SelectionBox   (selectionBox_)

name :: JSString
name = "node-editor"


nodeEditor :: Ref NodeEditor -> ReactView ()
nodeEditor ref = React.defineControllerView name ref $ \store () -> do

    let ne = store ^. dt
        transformMatrix = ne ^. NodeEditor.screenTransform . CoordsTransformation.logicalToScreen
        transform       = showTransformMatrix transformMatrix
    div_

        [ "className"   $= "graph"
        , onMouseDown   $ \_ e -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        ] $ do
        svg_
            [ "className" $= "plane plane-connections" ] $ do
            defs_ [] $ do
                el "filter"
                    [ "id"     $= "textShadow" ] $ do
                        el "feOffset"
                            [ "result" $= "offOut"
                            , "in"     $= "SourceAlpha"
                            , "dx"     $= "0"
                            , "dy"     $= "0"
                            ] mempty
                        el "feGaussianBlur"
                            [ "result"       $= "blurOut"
                            , "in"           $= "offOut"
                            , "stdDeviation" $= "2"
                            ] mempty
                        el "feBlend"
                            [ "in"   $= "SourceGraphic"
                            , "in2"  $= "blurOut"
                            , "mode" $= "normal"
                            ] mempty

            g_ [ "className" $= "connections"
               , "style"   @= Aeson.object [ "transform" Aeson..= transform ]
               ] $ do
                forM_ (store ^. dt . NodeEditor.connections . to HashMap.elems) $ \connectionRef -> connection_ connectionRef
                forM_ (store ^. dt . NodeEditor.currentConnection) $ \connectionRef -> currentConnection_ connectionRef
                selectionBox_ (store ^. dt . NodeEditor.selectionBox)
        div_
            [ "className" $= "plane plane--nodes"
            , "style"     @= Aeson.object [ "transform" Aeson..= transform ]
            ] $ do
                forM_ (store ^. dt . NodeEditor.nodes . to HashMap.elems) $ \nodeRef -> node_ nodeRef

nodeEditor_ :: Ref NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref = React.view (nodeEditor ref) () mempty
