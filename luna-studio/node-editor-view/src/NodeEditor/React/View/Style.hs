{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Style where

import           Common.Prelude
import qualified Data.Aeson                       as Aeson
import           Data.List                        (intercalate)
import           React.Flux                       as React

prefix :: JSString -> JSString
prefix a = (<>) "luna-" a

prefixFromList :: [String] -> JSString
prefixFromList a = fromString $ intercalate " " $ map ((<>) "luna-") a

blurBackground_ :: ReactElementM ViewEventHandler ()
blurBackground_ = div_
    [ "key"       $= "blurBackground"
    , "className" $= prefix "blur"
    ] mempty

selectionMark_ :: Double -> ReactElementM ViewEventHandler ()
selectionMark_ height = rect_
    [ "key"       $= "selectionMark"
    , "className" $= prefix "selection"
    , "style"     @= Aeson.object [ "height" Aeson..= ((show $ height) <> "px")]
    ] mempty

errorMark_ :: ReactElementM ViewEventHandler ()
errorMark_ = div_
    [ "key"       $= "errorMark"
    , "className" $= prefix "error"
    ] $ div_ mempty

plainRect_ :: JSString -> Double -> Double -> Double -> Double -> ReactElementM ViewEventHandler ()
plainRect_ key w h x y = React.viewWithSKey plainRect key (w, h, x, y) mempty

plainRect :: ReactView (Double, Double, Double, Double)
plainRect = React.defineView "plain-rect" $ \(w, h, x, y) -> do
    rect_ [ "width"  $= fromString (show w)
          , "height" $= fromString (show h)
          , "x"      $= fromString (show x)
          , "y"      $= fromString (show y)
          ] mempty

plainPath_ :: JSString -> JSString -> ReactElementM ViewEventHandler ()
plainPath_ c d = React.viewWithSKey plainPath "plain-path" (c, d) mempty

plainPath :: ReactView (JSString, JSString)
plainPath = React.defineView "plain-path" $ \(c, d) ->
    path_ [ "className" $= c, "d" $= d ] mempty


iconEye, iconEyeDisabled :: JSString
iconEye         = "M12 4.5c-5 0-9.3 3-11 7.5 1.7 4.4 6 7.5 11 7.5s9.3-3 11-7.5c-1.7-4.4-6-7.5-11-7.5zM12 17c-2.8 0-5-2.2-5-5s2.2-5 5-5 5 2.2 5 5-2.2 5-5 5zm0-8c-1.7 0-3 1.3-3 3s1.3 3 3 3 3-1.3 3-3-1.3-3-3-3z"
iconEyeDisabled = "M12 7c2.8 0 5 2.2 5 5 0 .7 0 1.3-.4 1.8l3 3c1.5-1.3 2.7-3 3.4-4.8-1.7-4.4-6-7.5-11-7.5-1.4 0-2.7.3-4 .7l2.2 2.2C10.7 7 11.4 7 12 7zM2 4.3l2.3 2.2.4.5C3 8.3 1.7 10 1 12c1.7 4.4 6 7.5 11 7.5 1.6 0 3-.3 4.4-.8l.4.4 3 3 1.2-1L3.3 3 2 4.3zm5.5 5.5L9 11.4v.6c0 1.7 1.3 3 3 3h.7l1.5 1.5c-.7.3-1.4.5-2.2.5-2.8 0-5-2.2-5-5 0-.8.2-1.5.5-2.2zm4.3-.8l3.2 3.2V12c0-1.6-1.3-3-3-3h-.2z"
