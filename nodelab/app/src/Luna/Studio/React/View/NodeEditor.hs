{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeEditor where


import qualified Data.HashMap.Strict                   as HashMap
import           React.Flux                            hiding (transform)
import qualified React.Flux                            as React
import           React.Flux.Internal                   (el)

import           JS.Scene                              (sceneId)
import qualified Luna.Studio.Data.CameraTransformation as CameraTransformation
import           Luna.Studio.Data.Matrix               (matrix3dPropertyValue)
import qualified Luna.Studio.Event.UI                  as UI
import           Luna.Studio.Prelude                   hiding (transform)
import qualified Luna.Studio.React.Event.NodeEditor    as NE
import           Luna.Studio.React.Model.App           (App)
import           Luna.Studio.React.Model.Node          (isEdge)
import           Luna.Studio.React.Model.NodeEditor    (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.React.Store               (Ref, dispatch)
import           Luna.Studio.React.View.Connection     (connection_, currentConnection_)
import           Luna.Studio.React.View.ConnectionPen  (connectionPen_)
import           Luna.Studio.React.View.Edge           (edgeSidebar_)
import           Luna.Studio.React.View.Node           (nodeDynamicStyles_, node_)
import           Luna.Studio.React.View.SelectionBox   (selectionBox_)
import           Luna.Studio.React.View.Visualization  (pinnedVisualization_)


name :: JSString
name = "node-editor"

nodeEditor_ :: Ref App -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty

nodeEditor :: ReactView (Ref App, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne) -> do
    let camera         = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        (edges, nodes) = partition isEdge $ ne ^. NodeEditor.nodes . to HashMap.elems
    div_
        [ "className" $= "luna-graph"
        , "id"        $= sceneId
        , "key"       $= "graph"
        , onMouseDown $ \_ e   -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        , onWheel     $ \e m w -> preventDefault e : dispatch ref (UI.NodeEditorEvent $ NE.Wheel m w)
        , onScroll    $ \e     -> [preventDefault e]
        ] $ do
        style_ [] $ do
            elemString $ ".luna-node-trans { transform: " <> matrix3dPropertyValue camera <> " }"
            forM_ nodes $ nodeDynamicStyles_ camera
        svg_
            [ "className" $= "luna-plane luna-plane-connections luna-node-trans"
            , "key"       $= "connections"
            ] $ do
            defs_
                [ "key" $= "defs" ] $
                el "filter"
                    [ "id"  $= "textShadow"
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
                , "className" $= "luna-connections"
                ] $ do
                mapM_ (uncurry (connection_ ref)) $ ne ^. NodeEditor.connections . to HashMap.toList
                mapM_ currentConnection_          $ ne ^. NodeEditor.currentConnection
                mapM_ selectionBox_               $ ne ^. NodeEditor.selectionBox
                mapM_ connectionPen_              $ ne ^. NodeEditor.connectionPen
        div_
            [ "className" $= "luna-plane luna-plane--nodes"
            , "key"       $= "nodes"
            ] $ do
            forM_ nodes                             $ node_ ref
            forM_ (ne ^. NodeEditor.visualizations) $ pinnedVisualization_ ref ne
        forM_ edges $ edgeSidebar_ ref
        canvas_
            [ "className" $= "luna-plane plane--canvas luna-hide"
            , "key"       $= "canvas"
            ] mempty
