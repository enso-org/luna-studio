{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeEditor where

import qualified Data.Aeson                            as Aeson
import qualified Data.HashMap.Strict                   as HashMap
import           React.Flux
import qualified React.Flux                            as React
import           React.Flux.Internal                   (el)

import qualified Event.UI                              as UI
import qualified Luna.Studio.Data.CameraTransformation as CameraTransformation
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
        transformMatrix = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        transform       = showTransformMatrix transformMatrix
    div_
        [ "className" $= "graph"
        , "id"        $= "Graph"
        , "key"       $= "graph"
        , onMouseDown $ \_ e   -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        , onWheel     $ \_ m w -> dispatch ref $ UI.NodeEditorEvent $ NE.Wheel m w
        ] $ do
        svg_
            [ "className" $= "plane plane-connections"
            , "style"     @= Aeson.object [ "transform" Aeson..= transform ]
            , "key"       $= "connections"
            ] $ do
            defs_ ["key" $= "defs"] $ do
                el "filter" [ "id" $= "textShadow"
                            , "key" $= "textShadow" ] $ do
                    el "feOffset"
                        [ "result" $= "offOut"
                        , "in"     $= "SourceAlpha"
                        , "dx"     $= "0"
                        , "dy"     $= "0"
                        , "key"    $= "feOffset"
                        ] mempty
                    el "feGaussianBlur"
                        [ "result"       $= "blurOut"
                        , "in"           $= "offOut"
                        , "stdDeviation" $= "2"
                        , "key"          $= "feGaussianBlur"
                        ] mempty
                    el "feBlend"
                        [ "in"   $= "SourceGraphic"
                        , "in2"  $= "blurOut"
                        , "mode" $= "normal"
                        , "key"  $= "feBlend"
                        ] mempty

            g_ [ "key"       $= "connections"
               , "className" $= "connections" ] $ do
                forM_ (store ^. dt . NodeEditor.connections . to HashMap.toList) $ uncurry connection_
                forM_ (store ^. dt . NodeEditor.currentConnection) $ \connectionRef -> currentConnection_ connectionRef
                selectionBox_ (store ^. dt . NodeEditor.selectionBox)
        div_
            [ "className" $= "plane plane--nodes"
            , "key"       $= "nodes"
            , "style"     @= Aeson.object [ "transform" Aeson..= transform ]
            ] $ do
                forM_ (store ^. dt . NodeEditor.nodes . to HashMap.toList) $ uncurry node_

nodeEditor_ :: Ref NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref = React.viewWithSKey (nodeEditor ref) name () mempty
