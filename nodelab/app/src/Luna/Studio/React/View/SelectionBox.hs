{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.SelectionBox where

import qualified Data.Aeson                           as Aeson
import           Luna.Studio.Data.Vector              (Position (Position), Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import           React.Flux
import qualified React.Flux                           as React
import           Luna.Studio.React.Model.SelectionBox (SelectionBox, end, start, visible)


name :: JSString
name = "selection-box"

selectionBox :: ReactView SelectionBox
selectionBox = React.defineView name $ \model -> do
    let pos       = Position (Vector2 (min (model ^. start . x) (model ^. end . x)) (min (model ^. start . y) (model ^. end . y)))
        width     = abs $ model ^. start . x - model ^. end . x
        height    = abs $ model ^. start . y - model ^. end . y
        translate = fromString $ "translate(" <> show (pos ^. x) <> "," <> show (pos ^. y) <> ")"
    when (model ^. visible) $
        rect_
            [ "width"     $= fromString (show width)
            , "height"    $= fromString (show height)
            , "style"     @= Aeson.object
                [ "strokeWidth"  Aeson..= ("3" :: String)
                , "stroke"       Aeson..= ("rgb(255,255,255)" :: String)
                , "opacity"      Aeson..= ("0.2" :: String)
                ]
            , "transform" $= translate
            ] mempty

selectionBox_ :: SelectionBox -> ReactElementM ViewEventHandler ()
selectionBox_ model = React.viewWithSKey selectionBox name model mempty
