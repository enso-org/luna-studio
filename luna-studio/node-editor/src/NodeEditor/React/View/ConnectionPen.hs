{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ConnectionPen where

import           Common.Prelude
import           NodeEditor.React.Model.ConnectionPen (ConnectionPen)
import qualified NodeEditor.React.Model.ConnectionPen as ConnectionPen
import           React.Flux
import qualified React.Flux                            as React

name :: JSString
name = "connectionPen"

connectionPen :: ReactView ConnectionPen
connectionPen = React.defineView name $ \model -> do
    let path  = model ^. ConnectionPen.path
        color = model ^. ConnectionPen.color
    path_
        [ "className"    $= "connectionPen"
        , "key"          $= "connectionPen"
        , "d"            $= convert path
        , "fill"         $= "transparent"
        , "stroke"       $= convert color
        , "strokeWidth"  $= "2"
        ] mempty


connectionPen_ :: ConnectionPen -> ReactElementM ViewEventHandler ()
connectionPen_ model = React.viewWithSKey connectionPen "connectionPen" model mempty
