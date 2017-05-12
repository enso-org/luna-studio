{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Project.ExportProject where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           LunaStudio.Data.Project (ProjectId)
import           Prologue


data Request = Request { _projectId :: ProjectId
                       } deriving (Eq, Generic, NFData, Show)

data Result = Result   { _projectData :: Text
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix = "empire.project.export"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
