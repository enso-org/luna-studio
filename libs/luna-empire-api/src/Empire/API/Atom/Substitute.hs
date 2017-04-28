{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Atom.Substitute where

import           Data.Binary             (Binary)
import           Prologue

import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request { _filePath :: FilePath
                       , _start    :: Int
                       , _end      :: Int
                       , _newText  :: Text
                       , _cursor   :: Maybe Int
                       } deriving (Generic, Eq, NFData, Show)

type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

data Update = Update { _filePath' :: FilePath
                     , _start'    :: Int
                     , _end'      :: Int
                     , _newText'  :: Text
                     , _cursor'   :: Maybe Int
                     , _tags      :: [(Int, [String])]
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update

topicPrefix = "empire.atom.file.substitute"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update               where topic _ = topicPrefix <> T.response
