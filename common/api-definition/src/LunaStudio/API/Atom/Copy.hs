module LunaStudio.API.Atom.Copy where

import Prologue

import qualified LunaStudio.API.Graph.Request as Request
import qualified LunaStudio.API.Response      as Response
import qualified LunaStudio.API.Topic         as Topic

import Data.Aeson.Types              (ToJSON)
import Data.Binary                   (Binary)
import Data.Path                     (File, Path, Rel)
import LunaStudio.Data.GraphLocation (GraphLocation (..))
import LunaStudio.Data.Range         (Range)

data Request = Request
    { _filePath :: Path Rel File
    , _span     :: [Range]
    } deriving (Eq, Generic, Show)

data Result = Result
    { _code :: Text }
    deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result


type Response = Response.Response Request () Result
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = Result

instance Topic.MessageTopic Request where
    topic = "empire.atom.file.copy"

instance Request.GraphRequest Request where
    location = lens getter setter where
        getter (Request file _) = GraphLocation file mempty
        setter (Request _ spans) (GraphLocation file _) = Request file spans

