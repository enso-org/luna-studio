module LunaStudio.API.Persistence.Project where

import           Data.Aeson.Types                   (FromJSON, ToJSON)
import           Data.Binary                        (Binary)
import           Data.IntMap.Lazy                   (IntMap)
import           LunaStudio.API.Persistence.Library (Library)
import           Prologue


type ProjectId = Int

data Project = Project
    { _name     :: String
    , _libs     :: IntMap Library
    } deriving (Eq, Generic, Show)

makeLenses ''Project

instance Binary   Project
instance NFData   Project
instance FromJSON Project
instance ToJSON   Project
