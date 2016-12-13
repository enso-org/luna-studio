module UI.Widget.Slider where

import           Luna.Studio.Prelude  hiding (span)

import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))

import           UI.Widget          (UIWidget)

newtype Slider = Slider { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Slider

foreign import javascript safe "new Slider($1, $2, $3)"  create'        :: Int    -> Double    -> Double           -> IO Slider
foreign import javascript safe "$1.setValue($2)"         setValue'      :: Slider -> Double                        -> IO ()
foreign import javascript safe "$1.setLabel($2)"         setLabel'      :: Slider -> JSString                      -> IO ()
foreign import javascript safe "$1.setValueLabel($2)"    setValueLabel' :: Slider -> JSString                      -> IO ()
foreign import javascript safe "$1.setFocus($2)"         setFocus'      :: Slider -> Bool                          -> IO ()
foreign import javascript safe "$1.setTicks($2, $3, $4)" setTicks'      :: Slider -> Bool      -> Double -> Double -> IO ()

log10 :: Double -> Double
log10 = logBase 10

limitTicks :: Int -> Int -> Double -> (Double, Double)
limitTicks minv maxv width = (offset, span) where
    range  = fromIntegral $ maxv - minv
    step   = 10.0 ** fromIntegral (floor $ log10 (range - 1.0) :: Integer)
    steps  = range / step
    span   = width / steps
    offset = fromIntegral (-minv) / range * width
