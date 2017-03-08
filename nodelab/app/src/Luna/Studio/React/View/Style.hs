{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Style where

import Luna.Studio.Prelude
import Data.List           (intercalate)
import React.Flux

prefix :: JSString -> JSString
prefix a = (<>) "luna-" a

prefixFromList :: [String] -> JSString
prefixFromList a = fromString $ intercalate " " $ map ((<>) "luna-") a

blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_
    [ "key"       $= "blurBackground"
    , "className" $= prefix "blur"
    ] mempty

selectionMark_ :: ReactElementM ViewEventHandler ()
selectionMark_ = div_
    [ "key"       $= "selectionMark"
    , "className" $= prefix "selection"
    ] mempty

plainRect :: Double -> Double -> Double -> Double -> ReactElementM ViewEventHandler ()
plainRect w h x y =
    rect_ [ "width"  $= fromString (show w)
          , "height" $= fromString (show h)
          , "x"      $= fromString (show x)
          , "y"      $= fromString (show y)
          ] mempty

plainPath :: JSString -> JSString -> ReactElementM ViewEventHandler ()
plainPath c d = path_ [ "className" $= c, "d" $= d ] mempty


-- TODO
portSelectablePath :: JSString -> Double -> Double -> Double -> ReactElementM ViewEventHandler ()
portSelectablePath c x y r = path_ [ "className" $= c, "d" $= fromString d ] mempty
    where d = "M 20 0 A " <> show r <> " " <> show r <> " 0 0 1 20 16 L 10 16 A " <> show r <> " " <> show r <> " 0 0 1 10 0 Z"
