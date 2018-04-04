module LunaStudio.Data.Library where

import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Prologue


type LibraryId = Int

data Library = Library
    { _name    :: Maybe String
    , _path    :: String
    } deriving (Eq, Generic, Show)

makeLenses ''Library

instance Binary Library
instance NFData Library
instance ToJSON Library
