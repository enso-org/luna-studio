{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.Graphics where

import qualified Graphics.API                          as GR
import           Luna.Studio.Prelude
import           React.Flux                            hiding (label_)


graphics_ :: Int -> GR.Geometry -> ReactElementM ViewEventHandler ()
graphics_ visIx (GR.Geometry material trans surface) =
    return () --TODO
