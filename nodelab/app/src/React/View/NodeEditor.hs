{-# LANGUAGE OverloadedStrings #-}
module React.View.NodeEditor where

import qualified Data.HashMap.Strict     as HashMap
import           React.Flux
import qualified React.Flux              as React
import           Utils.PreludePlus

import qualified Event.UI                as UI
import qualified React.Event.NodeEditor  as NE
import           React.Store             (Ref, dispatch, dt)
import           React.Model.NodeEditor  (NodeEditor)
import qualified React.Model.NodeEditor  as NodeEditor
import           React.View.Connection   (connection_)
import           React.View.Node         (node_)
import           React.View.SelectionBox (selectionBox_)


name :: JSString
name = "node-editor"


nodeEditor :: Ref NodeEditor -> ReactView ()
nodeEditor ref = React.defineControllerView name ref $ \store () -> do
    svg_
        [ "className"   $= "graph"
        , "xmlns"       $= "http://www.w3.org/2000/svg"
        , "xmlnsXlink"  $= "http://www.w3.org/1999/xlink"
        , onMouseDown   $ \_ e -> dispatch ref $ UI.NodeEditorEvent $ NE.MouseDown e
        ]
        $ do
        g_
            [ "className" $= "scene"
            , "transform" $= "matrix(1,0,0,1,0,0)" --TODO: Apply zooming and panning https://developer.mozilla.org/en/docs/Web/SVG/Attribute/transform
            ] $ do
                forM_ (store ^. dt . NodeEditor.nodes . to HashMap.elems) $ \nodeRef -> do
                    node_ nodeRef
                forM_ (store ^. dt . NodeEditor.connections . to HashMap.elems) $ \connectionRef -> do
                    connection_ connectionRef
                selectionBox_ (store ^. dt . NodeEditor.selectionBox)

nodeEditor_ :: Ref NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref = React.view (nodeEditor ref) () mempty
