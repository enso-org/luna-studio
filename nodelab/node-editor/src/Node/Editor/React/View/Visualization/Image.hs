{-# LANGUAGE OverloadedStrings #-}
module Node.Editor.React.View.Visualization.Image where

import           Luna.Prelude
import           Node.Editor.React.Model.Image (Image)
import           React.Flux


image_ :: Int -> Image -> ReactElementM ViewEventHandler ()
image_ _visIx df = div_ $ elemString $ show df
