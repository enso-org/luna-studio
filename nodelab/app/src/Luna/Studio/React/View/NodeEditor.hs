{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeEditor where


import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.Matrix                           as Matrix
import           Data.Maybe                            (mapMaybe)
import qualified Empire.API.Data.MonadPath             as MonadPath
import           JS.Scene                              (sceneId)
import qualified Luna.Studio.Data.CameraTransformation as CameraTransformation
import           Luna.Studio.Data.Matrix               (matrix3dPropertyValue)
import qualified Luna.Studio.Event.UI                  as UI
import           Luna.Studio.Prelude                   hiding (transform)
import qualified Luna.Studio.React.Event.NodeEditor    as NE
import           Luna.Studio.React.Model.App           (App)
import           Luna.Studio.React.Model.Constants     (connectionWidth)
import           Luna.Studio.React.Model.Node          (isEdge)
import           Luna.Studio.React.Model.NodeEditor    (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.React.Store               (Ref, dispatch)
import           Luna.Studio.React.View.Connection     (connection_, currentConnection_)
import           Luna.Studio.React.View.ConnectionPen  (connectionPen_)
import           Luna.Studio.React.View.Edge           (edgeSidebar_)
import           Luna.Studio.React.View.Monad          (monad_)
import           Luna.Studio.React.View.Node           (nodeDynamicStyles_, node_)
import           Luna.Studio.React.View.Searcher       (searcher_)
import           Luna.Studio.React.View.SelectionBox   (selectionBox_)
import qualified Luna.Studio.React.View.Style          as Style
import           Luna.Studio.React.View.Visualization  (pinnedVisualization_)
import           React.Flux                            hiding (transform)
import qualified React.Flux                            as React

name :: JSString
name = "node-editor"

nodeEditor_ :: Ref App -> NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref ne = React.viewWithSKey nodeEditor name (ref, ne) mempty

nodeEditor :: ReactView (Ref App, NodeEditor)
nodeEditor = React.defineView name $ \(ref, ne) -> do
    let camera         = ne ^. NodeEditor.screenTransform . CameraTransformation.logicalToScreen
        (edges, nodes) = partition isEdge $ ne ^. NodeEditor.nodes . to HashMap.elems
        lookupNode m   = ( m ^. MonadPath.monadType
                         , m ^. MonadPath.path . to (mapMaybe $ flip HashMap.lookup $ ne ^. NodeEditor.nodes))
        monads         = map lookupNode $ ne ^. NodeEditor.monads
        scale          = (Matrix.toList camera)!!0 :: Double
    div_
        [ "className" $= Style.prefix "graph"
        , "id"        $= sceneId
        , "key"       $= "graph"
        , onMouseDown $ \_ e   -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        , onWheel     $ \e m w -> preventDefault e : dispatch ref (UI.NodeEditorEvent $ NE.Wheel m w)
        , onScroll    $ \e     -> [preventDefault e]
        ] $ do
        style_ [ "key" $= "style" ] $ do
            elemString $ ".luna-selection  { box-shadow: 0 0 0 " <> show (0.52/(scale**1.5)) <> "px orange !important }"
            elemString $ ".luna-node-trans { transform: " <> matrix3dPropertyValue camera <> " }"
            elemString $ ".luna-connection__line { stroke-width: " <> show connectionWidth <> " }"
            forM_ (ne ^. NodeEditor.nodes . to HashMap.elems) $ nodeDynamicStyles_ camera
        svg_
            [ "className" $= Style.prefixFromList [ "plane", "plane--monads" ]
            , "key"       $= "monads"
            ] $
            g_
                [ "className" $= Style.prefixFromList [ "monads", "node-trans" ]
                ] $ forKeyed_ monads $ monad_ (length monads)
        svg_
            [ "className" $= Style.prefixFromList [ "plane", "plane-connections" ]
            , "key"       $= "connections"
            ] $
            g_
                [ "key"       $= "connections"
                , "className" $= Style.prefixFromList [ "connections", "node-trans" ]
                ] $ do
                mapM_ (uncurry (connection_ ref))  $ ne ^. NodeEditor.connections . to HashMap.toList
                mapM_ (uncurry (connection_ ref))  $ ne ^. NodeEditor.portDragConnections . to HashMap.toList
                mapM_ (uncurry currentConnection_) $ keyed $ ne ^. NodeEditor.currentConnections
                mapM_ selectionBox_                $ ne ^. NodeEditor.selectionBox
                mapM_ connectionPen_               $ ne ^. NodeEditor.connectionPen
        div_
            [ "className" $= Style.prefixFromList [ "plane", "plane--nodes" ]
            , "key"       $= "nodes"
            ] $ do
            forM_ nodes $ node_ ref
            forM_ (ne ^. NodeEditor.visualizations) $ pinnedVisualization_ ref ne
            mapM_ (searcher_ ref camera) $ ne ^. NodeEditor.searcher
        forM_ edges $ edgeSidebar_ ref (ne ^. NodeEditor.draggedPort)
        canvas_
            [ "className" $= Style.prefixFromList [ "plane", "plane--canvas", "hide" ]
            , "key"       $= "canvas"
            ] mempty
