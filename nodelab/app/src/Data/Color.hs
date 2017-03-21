module Data.Color where

import           Prelude

{- Color implementation based on https://github.com/gka/chroma.js -}

data LAB = LAB Float Float Float Float deriving (Show)
data LCH = LCH Float Float Float Float deriving (Show)
data RGB = RGB Float Float Float Float deriving (Show)

lab_Yn, lab_Xn, lab_Zn, lab_t0, lab_t1, lab_t2, lab_t3 :: Float
lab_Yn = 1
lab_Xn = 0.950470
lab_Zn = 1.088830
lab_t0 = 0.137931034
lab_t1 = 0.206896552
lab_t2 = 0.12841855
lab_t3 = 0.00885645

deg2rad :: Float -> Float
deg2rad x = x * pi / 180.0

limit :: Float -> Float -> Float -> Float
limit x l h
    | x < l = l
    | x > h = h
    | otherwise = x

limit256 :: Float -> Float
limit256 x = limit x 0 255

lch2rgb :: LCH -> RGB
lch2rgb = lab2rgb . lch2lab

lab2rgb :: LAB -> RGB
lab2rgb (LAB l a b alpha) = (RGB r' g' b' alpha) where
    y' = (l + 16) / 116
    x' = if isNaN a then y' else y' + a / 500
    z' = if isNaN b then y' else y' - b / 200
    y = lab_Yn * lab_xyz y'
    x = lab_Xn * lab_xyz x'
    z = lab_Zn * lab_xyz z'
    r' = limit256 $ xyz_rgb $ 3.2404542 * x - 1.5371385 * y - 0.4985314 * z
    g' = limit256 $ xyz_rgb $ -0.9692660 * x + 1.8760108 * y + 0.0415560 * z
    b' = limit256 $ xyz_rgb $ 0.0556434 * x - 0.2040259 * y + 1.0572252 * z

-- Convert from a qualitative parameter h and a quantitative parameter l to a 24-bit pixel.
-- These formulas were invented by David Dalrymple to obtain maximum contrast without going
-- out of gamut if the parameters are in the range 0-1.
--
-- A saturation multiplier was added by Gregor Aisch
lch2lab :: LCH -> LAB
lch2lab (LCH l c h alpha) = LAB l (cos h' * c)  (sin h' * c) alpha where
    h' = deg2rad h

lab_xyz :: Float -> Float
lab_xyz t
  | t > lab_t1 = t * t * t
  | otherwise  = lab_t2 * (t - lab_t0)

xyz_rgb :: Float -> Float
xyz_rgb r = 255 * if r <= 0.00304
    then 12.92 * r
    else 1.055 * (r ** (1 / 2.4)) - 0.055
