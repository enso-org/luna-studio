{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Image where

import           React.Flux

import           Luna.Studio.Prelude
import           Object.Widget.Plots.Image (Image)
import qualified Object.Widget.Plots.Image as Image



image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ visIx df = div_ $ elemString $ fromString $ show df
