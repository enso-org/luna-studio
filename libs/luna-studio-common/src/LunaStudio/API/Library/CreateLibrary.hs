{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Library.CreateLibrary where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           LunaStudio.Data.Library (Library, LibraryId)
import           LunaStudio.Data.Project (ProjectId)
import           Prologue


data Request = Request { _projectId   :: ProjectId
                       , _libraryName :: Maybe String
                       , _path        :: String
                       } deriving (Eq, Generic, Show)

data Result = Result   { _libraryId :: LibraryId
                       , _library   :: Library
                       } deriving (Eq, Generic, Show)

data Update = Update   { _libraryId' :: LibraryId
                       , _library'   :: Library
                       } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result
makeLenses ''Update
instance Binary Request
instance NFData Request
instance Binary Result
instance NFData Result
instance Binary Update
instance NFData Update


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.library.create"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update              where topic _ = topicPrefix <> T.update
