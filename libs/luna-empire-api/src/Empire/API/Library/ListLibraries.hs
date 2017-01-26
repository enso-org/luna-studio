module Empire.API.Library.ListLibraries where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Library (Library, LibraryId)
import           Empire.API.Data.Project (ProjectId)
import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request { _projectId :: ProjectId
                       } deriving (Generic, Eq, NFData, Show)

data Result = Result { _libraries :: [(LibraryId, Library)]
                     } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request Result
instance Response.ResponseResult Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result

topicPrefix = "empire.library.list"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
