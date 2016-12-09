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
        [ "className" $= "port port--self"
        , "fill"      $= "#8ABEB7"
        , "stroke"    $= "#8ABEB7"
        ] mempty


port_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
port_ ref p = React.view (port ref) p mempty
