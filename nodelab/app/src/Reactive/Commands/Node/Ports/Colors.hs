{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Ports.Colors
    ( colorPort
    , vtToColor
    ) where

import           Data.Hashable             (hash)
import           Utils.PreludePlus

import           Empire.API.Data.Port      (Port)
import qualified Empire.API.Data.Port      as Port
import           Empire.API.Data.TypeRep   (TypeRep (..))



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

vtToColor :: TypeRep -> Int
vtToColor = tpRepToColor

colorPort :: Port -> Int
colorPort port = vtToColor $ port ^. Port.valueType
