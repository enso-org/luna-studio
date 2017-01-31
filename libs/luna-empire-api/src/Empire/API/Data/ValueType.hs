{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Empire.API.Data.ValueType where

import           Control.DeepSeq         (NFData)
import           Data.Binary             (Binary)
import           Data.Hashable           (Hashable)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Prologue                hiding (Text, TypeRep)

import           Empire.API.Data.TypeRep (TypeRep (..))


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
