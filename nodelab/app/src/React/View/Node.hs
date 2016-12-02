{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where

import           React.Flux
import qualified React.Flux        as React
import           Utils.PreludePlus

import qualified Event.UI          as UI
import           React.Store       (Ref, dt)
import qualified React.Store       as Store
import           React.Store.Node  (Node)
import qualified React.Store.Node  as Node


name :: JSString
name = "node-editor"


node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        g_
            [ onClick $ \_ _ -> Store.dispatch nodeRef $ UI.NodeEvent Node.OnClick
            , "className" $= "node"
            , "viewbox"   $= "0 0 40 40"
            ] $ do
              circle_
                  [ "className"   $= "self"
                  , "cx"          $= "20"
                  , "cy"          $= "20"
                  , "r"           $= "5"
                  , "fill"        $= "green"
                  ] $ mempty
              circle_
                  [ "className"   $= "input"
                  , "cx"          $= "20"
                  , "cy"          $= "20"
                  , "r"           $= "18.5"
                  , "strokeWidth" $= "3px"
                  , "stroke"      $= "green"
                  , "fill"        $= "none"
                  ] $ mempty
              circle_
                  [ "className"   $= "output"
                  , "cx"          $= "20"
                  , "cy"          $= "20"
                  , "r"           $= "18.5"
                  , "strokeWidth" $= "3px"
                  , "stroke"      $= "#d20"
                  , "fill"        $= "none"
                  ] $ mempty


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
