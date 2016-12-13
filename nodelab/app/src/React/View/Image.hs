{-# LANGUAGE OverloadedStrings #-}
module React.View.Image where

import           React.Flux

import           Object.Widget.Plots.Image           (Image)
import qualified Object.Widget.Plots.Image           as Image
import           Utils.PreludePlus



image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ visIx df = text_ $ elemString $ fromString $ show df
