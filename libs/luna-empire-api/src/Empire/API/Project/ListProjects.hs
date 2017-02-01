{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Project.ListProjects where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Project (Project, ProjectId)
import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request deriving (Generic, Eq, NFData, Show)

data Result  = Result { _projects :: [(ProjectId, Project)]
                      } deriving (Generic, Eq, NFData, Show)

data Update  = Update { _projects' :: [(ProjectId, Project)]
                      } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result
makeLenses ''Update

instance Binary Request
instance Binary Result
instance Binary Update

topicPrefix = "empire.project.list"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where topic _ = topicPrefix <> T.update
