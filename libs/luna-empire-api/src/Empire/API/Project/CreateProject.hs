{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Empire.API.Project.CreateProject where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.Project       (ProjectId, Project)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _name :: String
                       } deriving (Generic, Show, Eq)

data Result = Result   { _projectId :: ProjectId
                       , _project   :: Project
                       } deriving (Generic, Show, Eq)


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

data Update = Update { _projectId' :: ProjectId
                     , _project'   :: Project
                     } deriving (Generic, Show, Eq)

makeLenses ''Request
makeLenses ''Result
makeLenses ''Update

instance Binary Request
instance Binary Result
instance Binary Update

topicPrefix = "empire.project.create"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
