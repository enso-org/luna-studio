module Luna.Studio.Data.HSL where

import           Luna.Studio.Data.Color (Color (Color))
import           Luna.Studio.Prelude



data HSL a = HSL { _h :: a
                 , _s :: a
                 , _l :: a
                 } deriving (Eq, Ord, Show)

makeLenses ''HSL

color :: Color -> HSL Float
color (Color 0) = HSL 0.0 0.0 0.5
color (Color i) = HSL (hue * 2.0 * pi) 0.6 0.5
    where
        hue = start + delta * (fromIntegral i)
        start = 90.6 / (2 * pi)
        steps = 16.0
        delta = 1.0 / steps

color' :: Color -> JSString
color' = toJSString . color

toJSString :: (Fractional a, Show a) => HSL a -> JSString
toJSString hsl = fromString $ "hsl(" <> show ((hsl ^. h) * 360.0) <> ","
                                     <> show ((hsl ^. s) * 100.0) <> "%,"
                                     <> show ((hsl ^. l) * 100.0) <> "%)"
