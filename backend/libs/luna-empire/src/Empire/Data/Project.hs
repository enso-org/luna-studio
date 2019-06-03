module Empire.Data.Project where

import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import qualified LunaStudio.Data.Project        as API
import qualified LunaStudio.API.Persistence.Library as Persistence
import qualified LunaStudio.API.Persistence.Project as Persistence
import           Empire.Data.Library            (Library)
import qualified Empire.Data.Library            as Library
import           Empire.Prelude

data Project = Project { _name     :: String
                       , _libs     :: IntMap Library
                       } deriving (Show)

make :: String -> Project
make name' = Project name' IntMap.empty

makeLenses ''Project
