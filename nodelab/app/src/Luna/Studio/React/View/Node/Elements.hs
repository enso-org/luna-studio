{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Elements where

import           Luna.Studio.Prelude
import           React.Flux


blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_ [ "key" $= "blurBackground", "className" $= "luna-blur" ] mempty

selectionMark_ :: ReactElementM ViewEventHandler ()
selectionMark_ = div_ [ "key" $= "selectionMark", "className" $= "luna-selection" ] mempty
