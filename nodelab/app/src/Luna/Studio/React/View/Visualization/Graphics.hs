{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Visualization.Graphics where

import qualified Graphics.API                          as GR
import           Luna.Studio.Prelude
import           React.Flux                            hiding (label_)


graphics_ :: Int -> GR.Graphics -> ReactElementM ViewEventHandler ()
graphics_ visIx (GR.Graphics layers) = forM_ layers layer_

layer_ :: GR.Layer -> ReactElementM ViewEventHandler ()
layer_ (GR.Layer geometry placement labels) = do
    return ()
    --TODO
