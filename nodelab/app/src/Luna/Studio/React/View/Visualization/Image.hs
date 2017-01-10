{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.Image where

import           Luna.Studio.Prelude
import           Object.Widget.Plots.Image (Image)
import           React.Flux


image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ _visIx df = div_ $ elemString $ fromString $ show df
