module LunaStudio.API.Request where

import Prologue

import qualified LunaStudio.API.Topic as Topic

import Data.Aeson.Types             (ToJSON)
import Data.Binary                  (Binary)
import Data.UUID.Types              (UUID)
import LunaStudio.API.Graph.Request (GraphRequest, location)


data Request a = Request
    { _requestId :: UUID
    , _guiID     :: Maybe UUID
    , _request   :: a
    } deriving (Eq, Generic, Show)

makeLenses ''Request
instance Binary a => Binary (Request a)
instance ToJSON a => ToJSON (Request a)

-- TODO: Refactor to `Request` and use as qualified name.
instance GraphRequest a => GraphRequest (Request a) where
    location = request . location

instance Topic.MessageTopic req => Topic.MessageTopic (Request req) where
    topic = Topic.topic @req <> Topic.request
