{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeEditor where

import qualified Data.Aeson                            as Aeson
import qualified Data.HashMap.Strict                   as HashMap
import qualified Luna.Studio.Data.CameraTransformation as CameraTransformation
import           Luna.Studio.Data.Matrix               (showTransformMatrixToSvg)
import qualified Luna.Studio.Event.UI                  as UI
import           Luna.Studio.Prelude                   hiding (transform)
import qualified Luna.Studio.React.Event.NodeEditor    as NE
import           Luna.Studio.React.Model.App           (App)
import           Luna.Studio.React.Model.NodeEditor    (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.React.Store               (Ref, dispatch)
import           Luna.Studio.React.View.Connection     (connection_, currentConnection_)
import           Luna.Studio.React.View.Node           (node_)
import           Luna.Studio.React.View.SelectionBox   (selectionBox_)
import           React.Flux
import qualified React.Flux                            as React
import           React.Flux.Internal                   (el)


name :: JSString
name = "node-editor"

nodeEditor :: ReactView (Ref App, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne) -> do
    let transformMatrix = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        transform       = showTransformMatrixToSvg transformMatrix
    div_
        [ "className" $= "graph"
        , "id"        $= "Graph"
        , "key"       $= "graph"
        , onMouseDown $ \_ e   -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        , onWheel     $ \e m w -> preventDefault e : dispatch ref (UI.NodeEditorEvent $ NE.Wheel m w)
        , onScroll    $ \e     -> [preventDefault e]
        ] $ do

        -- TODO: div_ [ "className" $= "plane plane--visuals" ] …

        svg_
            [ "className" $= "plane plane-connections"
            , "style"     @= Aeson.object [ "transform" Aeson..= transform ]
            , "key"       $= "connections"
            ] $ do
            defs_
                [ "key" $= "defs" ] $ do
                el "filter"
                    [ "id" $= "textShadow"
                    , "key" $= "textShadow"
                    ] $ do
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
            g_
                [ "key"       $= "connections"
                , "className" $= "connections"
                ] $ do
                mapM_ (uncurry (connection_ ref)) $ ne ^. NodeEditor.connections . to HashMap.toList
                mapM_ currentConnection_ $ ne ^. NodeEditor.currentConnection
                mapM_ selectionBox_ $ ne ^. NodeEditor.selectionBox
        div_
            [ "className" $= "plane plane--nodes"
            , "key"       $= "nodes"
            , "style"     @= Aeson.object [ "transform" Aeson..= transform ]
            ] $ do
            forM_ (ne ^. NodeEditor.nodes . to HashMap.elems) $ node_ ref
        canvas_
            [ "className" $= "plane plane--canvas hide"
            , "key"       $= "canvas"
            ] $ mempty


nodeEditor_ :: Ref App -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty
