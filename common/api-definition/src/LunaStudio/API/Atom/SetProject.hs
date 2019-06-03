module LunaStudio.API.Atom.SetProject where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue

import Path (Abs, Dir, Path)
import qualified Data.Binary        as Binary
import qualified Path

instance Binary (Path Abs Dir) where
    put = Binary.put . Path.toFilePath
    get = do
        filepath <- Binary.get
        case Path.parseAbsDir filepath of
            Nothing -> fail "Not an absolute directory"
            Just p  -> pure p

data Request = Request { _rootPath :: Path Abs Dir } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request


type Response = Response.SimpleResponse Request ()
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = ()

instance T.MessageTopic Request where
    topic = "empire.atom.project.set"
