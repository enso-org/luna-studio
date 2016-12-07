{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Empire.API.Library.CreateLibrary where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Response             as Response
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _projectId   :: ProjectId
                       , _libraryName :: Maybe String
                       , _path        :: String
                       } deriving (Generic, Show, Eq)

data Result = Result   { _libraryId :: LibraryId
                       , _library   :: Library
                       } deriving (Generic, Show, Eq)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

data Update = Update   { _libraryId' :: LibraryId
                       , _library'   :: Library
                       } deriving (Generic, Show, Eq)


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
