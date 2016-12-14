{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Data.Color
    ( colorPort
    , vtToColor
    , Color (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Hashable             (hash)
import           Luna.Studio.Prelude

import           Empire.API.Data.Port      (Port)
import qualified Empire.API.Data.Port      as Port
import           Empire.API.Data.TypeRep   (TypeRep (..))
import           Empire.API.Data.ValueType (ValueType (..))



newtype Color = Color { fromColor :: Int }
              deriving (Eq, Generic, Ord, Show)

instance FromJSON Color
instance ToJSON Color


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

vtToColor :: ValueType -> Color
vtToColor (TypeIdent t) = Color $ tpRepToColor t
vtToColor _ = Color 0

colorPort :: Port -> Color
colorPort port = vtToColor $ port ^. Port.valueType
