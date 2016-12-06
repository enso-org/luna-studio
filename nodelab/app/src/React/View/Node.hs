{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where


import qualified Data.Text.Lazy    as Text

import           React.Flux
import qualified React.Flux        as React

import qualified Event.UI          as UI
import           React.Store       (Ref, dt)
import qualified React.Store       as Store
import           React.Store.Node  (Node)
import qualified React.Store.Node  as Node
import           Utils.PreludePlus
import           Utils.Vector      (x, y)
import React.View.Port (port_)

name :: JSString
name = "node-editor"

--FIXME: move all styles to CSS
node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        let n = nodeStore ^. dt
            nodeId = n ^. Node.nodeId
            pos = n ^. Node.position
            translate = fromString $ "translate(" <> show (pos ^. x) <> "," <> show (pos ^. y) <> ")"
        g_
            [ onClick       $ \_ e -> Store.dispatch nodeRef $ UI.NodeEvent $ Node.Click e nodeId
            , onDoubleClick $ \_ _ -> Store.dispatch nodeRef $ UI.NodeEvent $ Node.Enter nodeId
            , onMouseDown   $ \_ e -> Store.dispatch nodeRef $ UI.NodeEvent $ Node.MouseDown e nodeId
            , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
            , "transform" $= translate
            , "key"       $= fromString (show nodeId)
            ] $ do
                circle_
                    [ "className" $= "selection-mark"
                    ] $ mempty
                circle_
                    [ "className"   $= "port port--self"
                    , "fill"        $= "#8ABEB7"
                    , "stroke"      $= "#8ABEB7"
                    ] $ mempty
                path_
                    [ "className"   $= "port port--i port--i--01"
                    , "fill"        $= "#8ABEB7"
                    , "stroke"      $= "#8ABEB7"
                    , "d"           $= "M18 0 A 20 20 1 0 0 0 18 H3 A 17 17 0 0 1 18 3 V0.1"
                    ] $ mempty
                path_
                    [ "className"   $= "port port--i port--i--02"
                    , "fill"        $= "#B5BD68"
                    , "stroke"      $= "#B5BD68"
                    , "d"           $= "M0 22 A 20 20 0 0 0 18 40 V37 A 17 17 0 0 1 3 22 H0.1"
                    ] $ mempty
                path_
                    [ "className"   $= "port port--o port--o--01"
                    , "fill"        $= "#B294BB"
                    , "stroke"      $= "#B294BB"
                    , "d"           $= "M22 0 A 20 20.1 0 0 1 22 40 V37 A 17 17.1 0 0 0 22 3 V0.1"
                    ] $ mempty
                text_
                    [ "className"   $= "name"
                    ] $ elemString $ Text.unpack $ n ^. Node.name
                forM_ (n ^. Node.ports) $ port_ nodeRef


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
