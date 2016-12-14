module UI.Widget.Number where

import           Luna.Studio.Prelude

import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))

import           Event.Keyboard     (KeyMods (..))
import           UI.Widget          (UIWidget)

newtype Number = Number { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Number

foreign import javascript safe "new LabeledWidget($1, $2, $3)" create'        :: Int    -> Double -> Double -> IO Number
foreign import javascript safe "$1.setLabel($2)"               setLabel'      :: Number -> JSString         -> IO ()
foreign import javascript safe "$1.setValueLabel($2)"          setValueLabel' :: Number -> JSString         -> IO ()
foreign import javascript safe "$1.setFocus($2)"               setFocus'      :: Number -> Bool             -> IO ()

setFocus :: Bool -> Number -> IO ()
setFocus = flip setFocus'

keyModMult :: KeyMods -> Double
keyModMult mods = case mods of
    KeyMods True  True  _ _ -> 1000.0
    KeyMods False True  _ _ ->  100.0
    KeyMods True  False _ _ ->   10.0
    _                       ->    1.0
