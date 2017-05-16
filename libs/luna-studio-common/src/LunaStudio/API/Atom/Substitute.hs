{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Atom.Substitute where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data Request = Request { _filePath :: FilePath
                       , _start    :: Int
                       , _end      :: Int
                       , _newText  :: Text
                       , _cursor   :: Maybe Int
                       } deriving (Eq, Generic, NFData, Show)

data Update = Update { _filePath' :: FilePath
                     , _start'    :: Int
                     , _end'      :: Int
                     , _newText'  :: Text
                     , _cursor'   :: Maybe Int
                     , _tags      :: [(Int, [String])]
                     } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance Binary Update


type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

topicPrefix = "empire.atom.file.substitute"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where topic _ = topicPrefix <> T.update
