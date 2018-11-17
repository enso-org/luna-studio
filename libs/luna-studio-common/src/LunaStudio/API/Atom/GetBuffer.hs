module LunaStudio.API.Atom.GetBuffer where

import Prologue

import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Data.Aeson.Types           (ToJSON)
import Data.Binary                (Binary)
import LunaStudio.Data.Breadcrumb (Breadcrumb (..))


data Request = Request { _filePath :: FilePath } deriving (Eq, Generic, Show)
data Result  = Result  { _code     :: Text     } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.atom.file.get"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response

instance G.GraphRequest Request where
    location = lens getter setter where
        getter (Request file)
            = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (Request _   ) (GraphLocation.GraphLocation file _)
            = Request file

