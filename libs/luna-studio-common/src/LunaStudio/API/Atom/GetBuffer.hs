{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Atom.GetBuffer where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data Request = Request { _filePath :: FilePath
                       , _span     :: Maybe [(Int, Int)]
                       } deriving (Eq, Generic, NFData, Show)

data Result  = Result { _code             :: Text
                      } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix = "empire.atom.file.get"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
