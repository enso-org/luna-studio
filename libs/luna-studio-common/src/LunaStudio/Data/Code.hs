module LunaStudio.Data.Code where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue


newtype Code = Code Text deriving (Eq, Generic, Show)

instance Binary   Code
instance NFData   Code
instance ToJSON   Code
instance FromJSON Code
instance Mempty   Code where
    mempty = Code mempty
