{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Visualization.Image where

import           Common.Prelude
import           NodeEditor.React.Model.Image (Image)
import           React.Flux


image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ _visIx df = div_ $ elemString $ show df
