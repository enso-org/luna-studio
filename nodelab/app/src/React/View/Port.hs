{-# LANGUAGE OverloadedStrings #-}
module React.View.Port where

import qualified Event.UI          as UI
import           React.Flux
import qualified React.Flux        as React
import           React.Store       (Ref, dispatch)
import           React.Store.Node   (Node)
import qualified React.Store.Node   as Node
import           Utils.PreludePlus
import Object.Widget.Port (Port)
import qualified Object.Widget.Port as Port


name :: JSString
name = "port"


port :: Ref Node -> ReactView Port
port ref = React.defineView name $ \port -> do
    circle_
        [ "className" $= "selection-mark"
        ] mempty
    circle_
        [ "className" $= "port port--self"
        , "fill"      $= "#8ABEB7"
        , "stroke"    $= "#8ABEB7"
        ] mempty
    path_
        [ "className" $= "port port--i port--i--01"
        , "fill"      $= "#8ABEB7"
        , "stroke"    $= "#8ABEB7"
        , "d"         $= "M18 0 A 20 20 1 0 0 0 18 H3 A 17 17 0 0 1 18 3 V0.1"
        ] mempty
    path_
        [ "className" $= "port port--i port--i--02"
        , "fill"      $= "#B5BD68"
        , "stroke"    $= "#B5BD68"
        , "d"         $= "M0 22 A 20 20 0 0 0 18 40 V37 A 17 17 0 0 1 3 22 H0.1"
        ] mempty
    path_
        [ "className" $= "port port--o port--o--01"
        , "fill"      $= "#B294BB"
        , "stroke"    $= "#B294BB"
        , "d"         $= "M22 0 A 20 20.1 0 0 1 22 40 V37 A 17 17.1 0 0 0 22 3 V0.1"
        ] mempty


port_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
port_ ref p = React.view (port ref) p mempty
