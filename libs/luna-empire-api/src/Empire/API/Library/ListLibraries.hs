module Empire.API.Library.ListLibraries where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Response             as Response
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _projectId :: ProjectId
                       } deriving (Generic, Show, Eq)

data Result = Result { _libraries :: [(LibraryId, Library)]
                     } deriving (Generic, Show, Eq)

-- data Inverse = Maybe ()

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result


topicPrefix = "empire.library.list"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
