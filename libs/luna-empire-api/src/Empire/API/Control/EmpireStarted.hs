module Empire.API.Control.EmpireStarted where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import qualified Empire.API.Topic              as T


data Status = Status { } deriving (Eq, Generic, NFData, Show)

makeLenses ''Status

instance Binary Status
instance T.MessageTopic Status  where topic _ = "empire.control.started.status"
