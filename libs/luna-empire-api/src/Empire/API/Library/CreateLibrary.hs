{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Empire.API.Library.CreateLibrary where

import           Data.Binary             (Binary)
import           Prologue

import           Empire.API.Data.Library (Library, LibraryId)
import           Empire.API.Data.Project (ProjectId)
import qualified Empire.API.Request      as R
import qualified Empire.API.Response     as Response
import qualified Empire.API.Topic        as T

data Request = Request { _projectId   :: ProjectId
                       , _libraryName :: Maybe String
                       , _path        :: String
                       } deriving (Generic, Eq, NFData, Show)

data Result = Result   { _libraryId :: LibraryId
                       , _library   :: Library
                       } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

data Update = Update   { _libraryId' :: LibraryId
                       , _library'   :: Library
                       } deriving (Generic, Eq, NFData, Show)


makeLenses ''Request
makeLenses ''Result
makeLenses ''Update

instance Binary Request
instance Binary Result
instance Binary Update

topicPrefix = "empire.library.create"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
