module LunaStudio.API.Control.EmpireStarted where

import           Data.Binary          (Binary)
import qualified LunaStudio.API.Topic as T
import           Prologue


data Status = Status { } deriving (Eq, Generic, NFData, Show)

makeLenses ''Status
instance Binary Status


instance T.MessageTopic Status where topic _ = "empire.control.started.status"
