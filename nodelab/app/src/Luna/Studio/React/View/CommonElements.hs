{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.CommonElements where

import           Luna.Studio.Prelude
import           React.Flux


blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_ [ "key" $= "blurBackground", "className" $= "blur" ] mempty
