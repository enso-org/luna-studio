{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.SelectionBox where

import qualified Data.Aeson               as Aeson
import           React.Flux
import qualified React.Flux               as React
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector             (Vector2 (Vector2), x, y)

import           Luna.Studio.React.Store              (Ref, dt)
import           Luna.Studio.React.Model.SelectionBox (SelectionBox, end, start, visible)



name :: JSString
name = "selection-box"


selectionBox :: Ref SelectionBox -> ReactView ()
selectionBox ref = React.defineControllerView name ref $ \store () -> do
    let sb = store ^. dt
        pos    = Vector2 (min (sb ^. start . x) (sb ^. end . x)) (min (sb ^. start . y) (sb ^. end . y))
        width  = abs $ sb ^. start . x - sb ^. end . x
        height = abs $ sb ^. start . y - sb ^. end . y
        translate = fromString $ "translate(" <> show (pos ^. x) <> "," <> show (pos ^. y) <> ")"

    when (sb ^. visible) $
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

selectionBox_ :: Ref SelectionBox -> ReactElementM ViewEventHandler ()
selectionBox_ ref = React.view (selectionBox ref) () mempty
