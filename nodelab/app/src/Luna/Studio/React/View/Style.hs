{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Style where

import Luna.Studio.Prelude

lunaPrefix :: JSString -> JSString
lunaPrefix = (<>) "luna-"
