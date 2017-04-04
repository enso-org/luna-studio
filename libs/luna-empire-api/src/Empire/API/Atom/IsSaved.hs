{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Atom.IsSaved where

import           Data.Binary             (Binary)
import           Prologue

import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data IsSaved = True | False deriving (Generic, Eq, NFData, Show)

data Request = Request { _filePath :: FilePath
                       } deriving (Generic, Eq, NFData, Show)

data Result  = Result { _status             :: IsSaved
                      } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result

instance Binary IsSaved
instance Binary Request
instance Binary Result

topicPrefix = "empire.atom.file.save"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
