{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Data.Color
    ( fromPort
    , Color (..)
    , fromType
    , h
    , l
    , s
    , buildLCH
    , buildHsl
    ) where

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Color              as Color
import           Data.Convert            (Convertible (convert))
import           Data.Fixed              (mod')
import           Data.Hashable           (hash)
import           Luna.Studio.Prelude

import           Empire.API.Data.Port    (Port)
import qualified Empire.API.Data.Port    as Port
import           Empire.API.Data.TypeRep (TypeRep (..))

newtype Color = Color { fromColor :: Int }
              deriving (Eq, Generic, Ord, Show, NFData)

instance FromJSON Color
instance ToJSON Color


data HSL a = HSL { _h :: a
                 , _s :: a
                 , _l :: a
                 } deriving (Eq, Ord, Show)

makeLenses ''HSL

buildHsl :: Color -> HSL Float
buildHsl (Color 0) = HSL 0.0 0.0 0.5
buildHsl (Color i) = HSL (hue * 2.0 * pi) 0.6 0.5
    where
        hue = start + delta * (fromIntegral i - 1)
        start = 90.6 / (2 * pi)
        steps = 16.0
        delta = 1.0 / steps

buildLCH :: Color -> Color.LCH
buildLCH (Color 0) = Color.LCH 50  0 0 255
buildLCH (Color i) = Color.LCH 30 45 h' 255 where
    h'    = (start + delta * (fromIntegral i - 1)) `mod'` 360
    start = 100.7
    delta = 256/pi

instance Convertible Color.LCH JSString where
    convert = convert . Color.lch2rgb

instance Convertible Color JSString where
    convert = convert . buildLCH

instance Convertible Color.RGB JSString where
    convert (Color.RGB r g b a) = convert $ "rgba(" <> show (round r :: Int)
                                             <> "," <> show (round g :: Int)
                                             <> "," <> show (round b :: Int)
                                             <> "," <> show (round a :: Int)
                                             <> ")"

instance (Fractional a, Show a) => Convertible (HSL a) JSString where
    convert hsl = convert $ "hsl(" <> show ((hsl ^. h) * 360.0) <> ","
                                   <> show ((hsl ^. s) * 100.0) <> "%,"
                                   <> show ((hsl ^. l) * 100.0) <> "%)"

hashMany :: [TypeRep] -> Int
hashMany as = sum $ zipWith (*) powers (tpRepToColor <$> as) where
    nums   = [0..] :: [Integer]
    powers = (37 ^) <$> nums

ensureRange :: Integral a => a -> a
ensureRange n = (n `mod` 8) + 1

tpRepToColor :: TypeRep -> Int
tpRepToColor (TCons tn as) = ensureRange $ case tn of
     "Int"        -> 0
     "Bool"       -> 1
     "Double"     -> 2
     "String"     -> 3
     "List"       -> 5 + hashMany as
     _            -> hash tn + hashMany as
tpRepToColor (TLam as out) = ensureRange . hashMany $ out : as
tpRepToColor (TVar _n) = 9
tpRepToColor _ = 0

fromType :: TypeRep -> Color
fromType = Color . tpRepToColor

fromPort :: Port -> Color
fromPort port = fromType $ port ^. Port.valueType
