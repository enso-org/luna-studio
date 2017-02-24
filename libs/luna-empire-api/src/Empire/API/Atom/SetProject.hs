{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Atom.SetProject where

import           Data.Binary             (Binary)
import           Prologue

import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request { _rootPath :: FilePath
                       } deriving (Generic, Eq, NFData, Show)

type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

makeLenses ''Request

instance Binary Request

topicPrefix = "empire.atom.project.set"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
