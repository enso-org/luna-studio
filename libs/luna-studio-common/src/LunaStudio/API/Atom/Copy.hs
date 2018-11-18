module LunaStudio.API.Atom.Copy where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           LunaStudio.Data.Range   (Range)
import           Prologue

data Request = Request
    { _filePath :: FilePath
    , _span     :: [Range]
    } deriving (Eq, Generic, Show)

data Result  = Result { _code :: Text } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result


type Response = Response.Response Request () Result
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = Result

instance T.MessageTopic Request where
    topic = "empire.atom.file.copy"
