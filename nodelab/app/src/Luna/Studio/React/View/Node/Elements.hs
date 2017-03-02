{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Elements where

import           Luna.Studio.Prelude
import qualified Luna.Studio.React.View.Style as Style
import           React.Flux


blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_
    [ "key" $= "blurBackground"
    , "className" $= Style.prefix "blur"
    ] mempty

selectionMark_ :: ReactElementM ViewEventHandler ()
selectionMark_ = div_
    [ "key" $= "selectionMark"
    , "className" $= Style.prefix "selection"
    ] mempty
