{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Project.ListProjects where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           LunaStudio.Data.Project (Project, ProjectId)
import           Prologue


data Request = Request deriving (Eq, Generic, NFData, Show)

data Result  = Result { _projects :: [(ProjectId, Project)]
                      } deriving (Eq, Generic, NFData, Show)

data Update  = Update { _projects' :: [(ProjectId, Project)]
                      } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result
makeLenses ''Update
instance Binary Request
instance Binary Result
instance Binary Update


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix = "empire.project.list"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where topic _ = topicPrefix <> T.update
