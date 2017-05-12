{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Atom.OpenFile where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data Request = Request { _filePath :: FilePath
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
instance Binary Request


type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

topicPrefix = "empire.atom.file.open"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
