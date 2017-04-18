{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.Image where

import           Luna.Prelude
import           Luna.Studio.React.Model.Image (Image)
import           React.Flux


image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ _visIx df = div_ $ elemString $ show df
