module LunaStudio.API.Library.CreateLibrary where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           LunaStudio.Data.Library (Library, LibraryId)
import           LunaStudio.Data.Project (ProjectId)
import           Prologue


data Request = Request
    { _projectId   :: ProjectId
    , _libraryName :: Maybe String
    , _path        :: String
    } deriving (Eq, Generic, Show)

data Result = Result
    { _libraryId :: LibraryId
    , _library   :: Library
    } deriving (Eq, Generic, Show)

data Update = Update
    { _libraryId' :: LibraryId
    , _library'   :: Library
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result
makeLenses ''Update

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result
instance Binary Update
instance NFData Update
instance ToJSON Update


type Response = Response.Response Request () Result
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = Result

instance T.MessageTopic Request where
    topic = "empire.library.create"
instance T.MessageTopic Update where
    topic = T.topic @Request <> T.update
