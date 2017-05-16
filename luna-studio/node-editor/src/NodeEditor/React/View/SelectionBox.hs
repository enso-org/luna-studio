{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.SelectionBox where

import           Common.Prelude
import qualified Data.Aeson                          as Aeson
import           LunaStudio.Data.Position            (fromDoubles, x, y)
import           NodeEditor.React.Model.SelectionBox (SelectionBox, end, start)
import           React.Flux
import qualified React.Flux                          as React


name :: JSString
name = "selection-box"

selectionBox :: ReactView SelectionBox
selectionBox = React.defineView name $ \model -> do
    let pos       = fromDoubles (min (model ^. start . x) (model ^. end . x)) (min (model ^. start . y) (model ^. end . y))
        width     = abs $ model ^. start . x - model ^. end . x
        height    = abs $ model ^. start . y - model ^. end . y
        translate = "translate(" <> jsShow (pos ^. x) <> "," <> jsShow (pos ^. y) <> ")"
    rect_
        [ "width"     $= jsShow width
        , "height"    $= jsShow height
        , "style"     @= Aeson.object
            [ "strokeWidth"  Aeson..= ("3" :: String)
            , "stroke"       Aeson..= ("rgb(255,255,255)" :: String)
            , "opacity"      Aeson..= ("0.2" :: String)
            ]
        , "transform" $= translate
        ] mempty

selectionBox_ :: SelectionBox -> ReactElementM ViewEventHandler ()
selectionBox_ model = React.viewWithSKey selectionBox name model mempty
