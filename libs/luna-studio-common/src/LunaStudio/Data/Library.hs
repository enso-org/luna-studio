module LunaStudio.Data.Library where

import           Prologue
import           Data.Binary (Binary)


type LibraryId = Int

data Library = Library { _name    :: Maybe String
                       , _path    :: String
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Library
instance Binary Library
