{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.ForeignObject where

import           React.Flux



foreignObject_ :: Term eventHandler arg result => arg -> result
foreignObject_ = term "foreignObject"
