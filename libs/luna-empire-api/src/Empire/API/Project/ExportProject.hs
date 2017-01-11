{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Project.ExportProject where

import           Data.Binary             (Binary)
import           Data.Text               (Text)
import           Prologue                hiding (Text)

import           Empire.API.Data.Project (Project, ProjectId)
import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request { _projectId :: ProjectId
                       } deriving (Generic, Show, Eq)

data Result = Result   { _projectData :: Text
                       } deriving (Generic, Show, Eq)

type Response = Response.Response Request Result
instance Response.ResponseResult Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result

topicPrefix = "empire.project.export"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
