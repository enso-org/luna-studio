{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.ConnectionPen where

import           Data.Curve                            (Curve, CurveSegment)
import qualified Data.Curve                            as Curve
import           Data.Position                         (Position, x, y)
import           Luna.Studio.Data.Color                (toJSString)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.ConnectionPen (ConnectionPen)
import qualified Luna.Studio.React.Model.ConnectionPen as ConnectionPen
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
        , "d"            $= fromString path
        , "fill"         $= "transparent"
        , "stroke"       $= toJSString color
        , "strokeWidth"  $= "2"
        ] mempty


connectionPen_ :: ConnectionPen -> ReactElementM ViewEventHandler ()
connectionPen_ model = React.viewWithSKey connectionPen "connectionPen" model mempty
