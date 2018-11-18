module LunaStudio.API.Control.EmpireStarted where

import           Data.Aeson.Types     (ToJSON)
import           Data.Binary          (Binary)
import qualified LunaStudio.API.Topic as T
import           Prologue


data Status = Status deriving (Eq, Generic, Show)

makeLenses ''Status

instance Binary Status
instance NFData Status
instance ToJSON Status


instance T.MessageTopic Status where topic = "empire.control.started.status"
