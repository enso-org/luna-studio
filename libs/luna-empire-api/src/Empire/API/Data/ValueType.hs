{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Empire.API.Data.ValueType where

import           Data.Binary             (Binary)
import           Data.Hashable           (Hashable)
import qualified Data.Text.Lazy          as Text
import           Prologue                hiding (TypeRep)

import           Empire.API.Data.TypeRep (TypeRep (..))


data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Show, Eq, Enum, Generic)

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
