{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Visualization.Graphics where

import qualified Graphics.API        as GR
import           Common.Prelude
import           React.Flux          hiding (label_)


graphics_ :: Int -> GR.Geometry -> ReactElementM ViewEventHandler ()
graphics_ _visIx (GR.Geometry _material _trans _surface) =
    return () --TODO
