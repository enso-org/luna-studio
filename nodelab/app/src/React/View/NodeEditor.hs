{-# LANGUAGE OverloadedStrings #-}
module React.View.NodeEditor where

import qualified Data.HashMap.Strict    as HashMap
import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import           React.Store            (Ref, dt)
import           React.Store.NodeEditor (NodeEditor)
import qualified React.Store.NodeEditor as NodeEditor
import           React.View.Connection  (connection_)
import           React.View.Node        (node_)


name :: JSString
name = "node-editor"


nodeEditor :: Ref NodeEditor -> ReactView ()
nodeEditor ref = React.defineControllerView name ref $ \store () -> do
    svg_
        [ "className"   $= "graph"
        , "width"       $= "800px"
        , "height"      $= "500px"
        , "xmlns"       $= "http://www.w3.org/2000/svg"
        , "xmlns:xlink" $= "http://www.w3.org/1999/xlink"
        ]
        $ do
        defs_
            $ do
                clipPath_ [ "id" $= "clipInput" ] $ do
                    rect_
                        [ "x"      $= "0"
                        , "y"      $= "0"
                        , "height" $= "40"
                        , "width"  $= "18"
                        ] $ do mempty
                clipPath_ [ "id" $= "clipOutput" ] $ do
                    rect_
                        [ "x"      $= "22"
                        , "y"      $= "0"
                        , "height" $= "40"
                        , "width"  $= "18"
                        ] $ do mempty

        forM_ (store ^. dt . NodeEditor.nodes . to HashMap.elems) $ \nodeRef -> do
            node_ nodeRef
        forM_ (store ^. dt . NodeEditor.connections . to HashMap.elems) $ \connectionRef -> do
            connection_ connectionRef

nodeEditor_ :: Ref NodeEditor -> ReactElementM ViewEventHandler ()
nodeEditor_ ref = React.view (nodeEditor ref) () mempty
