{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Atom.GetBuffer where

import           Data.Binary             (Binary)
import           Prologue

import qualified Empire.API.Request       as R
import qualified Empire.API.Response      as Response
import qualified Empire.API.Topic         as T

data Request = Request { _filePath :: FilePath
                       , _span     :: Maybe [(Int, Int)]
                       } deriving (Generic, Eq, NFData, Show)

data Result  = Result { _code             :: Text
                      , _tags             :: [(Int, [String])]
                      } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result

topicPrefix = "empire.atom.file.get"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
