module LunaStudio.Data.Range where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue


data Range = Range
    { _start :: Int
    , _end   :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''Range

instance Binary   Range
instance NFData   Range
instance FromJSON Range
instance ToJSON   Range

instance Convertible (Int, Int) Range where
    convert = uncurry Range
