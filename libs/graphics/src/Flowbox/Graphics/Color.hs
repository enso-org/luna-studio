---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate  as A

import           Flowbox.Graphics.Utils
import           Flowbox.Prelude        as P



data Color a = RGB  { r :: a, g :: a, b :: a         }
             | RGBA { r :: a, g :: a, b :: a, a :: a }
             | HSV  { h :: a, s :: a, v :: a         }
             | HSL  { h :: a, s :: a, v :: a         }
             | CMY  { c :: a, m :: a, y :: a         }
             | CMYK { c :: a, m :: a, y :: a, k :: a }
             | YUV  { y :: a, u :: a, v :: a }
             deriving (Show)


type ColorAcc a = Color (Exp a)

--helperHsvHsl :: Exp Int -> Exp a -> Exp a -> Int
helperHsvHsl :: (A.Elt a, A.Elt (A.Plain b), A.IsNum a, A.IsScalar a, A.Lift Exp b, Num b) => Exp a -> b -> b -> Exp (A.Plain b, A.Plain b, A.Plain b)
helperHsvHsl i x z = A.cond (i A.==* 0) (A.lift (x,   z,   x*0))
                   $ A.cond (i A.==* 1) (A.lift (z,   x,   x*0))
                   $ A.cond (i A.==* 2) (A.lift (x*0, x,   z))
                   $ A.cond (i A.==* 3) (A.lift (x*0, z,   x))
                   $ A.cond (i A.==* 4) (A.lift (z,   x*0, x))
                   $ A.cond (i A.==* 5) (A.lift (x,   x*0, z))
                   $ A.lift (x,   z,   x*0)

toRGB :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toRGB color@(RGB{}) = color
toRGB (RGBA r' g' b' _) = RGB r' g' b'
toRGB (HSV  h' s' v') = RGB (r'+m') (g'+m') (b'+m')
    where (r', g', b') = A.unlift res
          res = helperHsvHsl i c' x
          h'' = h' * 6
          i = (A.floor h'') `mod` 6 :: Exp (A.Plain Int)
          x = c' * (1 - abs(h'' `nonIntRem` 2 - 1))
          c' = v' * s'
          m' = v' - c'
toRGB (HSL h' s' l') = RGB (r'+m') (g'+m') (b'+m')
    where (r', g', b') = A.unlift res
          res = helperHsvHsl i c' x
          h'' = h' * 6
          i = (A.floor h'') `mod` 6 :: Exp (A.Plain Int)
          x = c' * (1 - abs(h'' `nonIntRem` 2 - 1))
          c' = (1 - abs(2 * l' - 1)) * s'
          m' = l' - c' / 2
toRGB (CMY c' m' y') = RGB r' g' b'
    where r' = 1 - c'
          g' = 1 - m'
          b' = 1 - y'
toRGB (CMYK c' m' y' k') = RGB r' g' b'
    where r' = (1 - c') * k''
          g' = (1 - m') * k''
          b' = (1 - y') * k''
          k'' = (1 - k')
-- INFO: the internet said something about clamping the values, but I guess it's enough to clamp them just before displaying the image
toRGB (YUV y' u' v') = RGB r' g' b'
    where r' = 1.164 * (y' - 0.0625) + 1.596 * (v' - 0.5)
          g' = 1.164 * (y' - 0.0625) - 0.813 * (v' - 0.5) - 0.391 * (u' - 0.5)
          b' = 1.164 * (y' - 0.0625) + 2.018 * (u' - 0.5)


toRGBA :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toRGBA color@(RGBA{}) = color
toRGBA (RGB r' g' b') = RGBA r' g' b' (r'*0 + 1)
toRGBA color          = toRGBA . toRGB $ color


toHSV :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toHSV color@(HSV{}) = color
toHSV (RGB r' g' b')   = HSV h'' s' v'
    where h'' = (h' A.>* 0 A.? (h' , h' + 6)) / 6
          h' = A.cond (delta A.==* 0) 0
             $ A.cond (r' A.==* maxRGB) (((g' - b') / delta) `nonIntRem` 6)
             $ A.cond (g' A.==* maxRGB) ((b' - r') / delta + 2)
             $ (r'-g') / delta + 4
          s' = delta A.==* 0 A.? (0, delta / maxRGB)
          v' = maxRGB
          minRGB = min r' $ min g' b'
          maxRGB = max r' $ max g' b'
          delta = maxRGB - minRGB
toHSV color = toHSV . toRGB $ color


toHSL :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toHSL color@(HSL{}) = color
toHSL (RGB r' g' b')   = HSL h'' s' l'
    where h'' = (h' A.>* 0 A.? (h' , h' + 6)) / 6
          h' = delta A.==* 0 A.? (0,
                r' A.==* maxRGB A.? (((g' - b') / delta) `nonIntRem` 6,
                g' A.==* maxRGB A.? ((b' - r') / delta + 2,
                (r'-g') / delta + 4
              )))
          s' = delta A.==* 0 A.? (0, delta / (1 - abs(2 * l' - 1)))
          l' = (maxRGB + minRGB) / 2
          minRGB = min r' $ min g' b'
          maxRGB = max r' $ max g' b'
          delta = maxRGB - minRGB
toHSL color = toHSL . toRGB $ color


toCMY :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toCMY color@(CMY{}) = color
toCMY (RGB r' g' b') = CMY c' m' y'
    where c' = 1 - r'
          m' = 1 - g'
          y' = 1 - b'
toCMY color = toCMY . toRGB $ color


toCMYK :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toCMYK color@(CMYK{}) = color
toCMYK (RGB r' g' b') = CMYK c' m' y' k'
    where c'  = (1 - r' - k') / k''
          m'  = (1 - g' - k') / k''
          y'  = (1 - b' - k') / k''
          k'  = 1 - maxRGB
          k'' = 1 - k'
          maxRGB = max r' $ max g' b'
toCMYK color = toCMYK . toRGB $ color

toYUV :: (A.Elt a, A.IsFloating a) => ColorAcc a -> ColorAcc a
toYUV color@(YUV{}) = color
toYUV (RGB r' g' b') = YUV y' u' v'
    where y' = 0.257 * r' + 0.504 * g' + 0.098 * b' + 0.0625
          u' = 0.148 * r' - 0.291 * g' + 0.439 * b' + 0.5
          v' = 0.439 * r' - 0.368 * g' - 0.071 * b' + 0.5
toYUV color = toYUV . toRGB $ color
