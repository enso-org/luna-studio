module LunaStudio.Data.Point where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue


data Point = Point
    { _column :: Int
    , _row    :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''Point

instance Binary   Point
instance NFData   Point
instance FromJSON Point
instance ToJSON   Point
instance Convertible (Int, Int) Point where
    convert = uncurry Point
