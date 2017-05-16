{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LunaStudio.Data.ValueType where

import           Data.Binary             (Binary)
import qualified Data.Text               as Text
import           LunaStudio.Data.TypeRep (TypeRep (..))
import           Prologue                hiding (Text, TypeRep)


data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Eq, Enum, Generic, Show)

instance Binary ValueTypeEnum

toEnum' :: TypeRep -> ValueTypeEnum
toEnum' (TCons name _) = case name of
  "Int"    -> DiscreteNumber
  "Long"   -> DiscreteNumber
  "Float"  -> ContinuousNumber
  "Double" -> ContinuousNumber
  "String" -> String
  "Bool"   -> Bool
  _        -> Other
toEnum' _ = Other

toEnum :: Getter TypeRep ValueTypeEnum
toEnum = to toEnum'

toText :: Getter TypeRep Text.Text
toText = to (Text.pack . toString)
