module LunaStudio.Data.Project where

import           Prologue
import           Data.Binary             (Binary)
import           Data.IntMap.Lazy        (IntMap)
import           Data.UUID.Types         (UUID)
import           LunaStudio.Data.Library (Library)


type ProjectId = UUID

data Project = Project { _name     :: String
                       , _libs     :: IntMap Library
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Project
instance Binary Project
