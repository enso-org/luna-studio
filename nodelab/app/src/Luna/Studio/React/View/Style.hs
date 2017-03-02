{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Style where

import Luna.Studio.Prelude
import Data.List           (intercalate)

prefix :: JSString -> JSString
prefix a = (<>) "luna-" a

prefixFromList :: [String] -> JSString
prefixFromList a = fromString $ intercalate " " $ map ((<>) "luna-") a
