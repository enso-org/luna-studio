module LunaStudio.API.Control.Interpreter where

import           Control.Lens                  (makePrisms)
import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           Prologue

data InterpreterCommand
    = Start
    | Pause
    | Reload
    deriving (Eq, Generic, Show)

data Request = Request
    { _location :: GraphLocation
    , _command  :: InterpreterCommand
    } deriving (Eq, Generic, Show)

data Update = Update Text deriving (Eq, Generic, Show)

makePrisms ''InterpreterCommand
makeLenses ''Request
makeLenses ''Update

instance Binary InterpreterCommand
instance NFData InterpreterCommand
instance ToJSON InterpreterCommand
instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Update
instance NFData Update
instance ToJSON Update

instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.control.interpreter"
instance T.MessageTopic Update  where
    topic = T.topic @Request <> T.update
