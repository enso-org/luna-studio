module LunaStudio.API.Request where

import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Data.UUID.Types  (UUID)
import           Prologue
import           LunaStudio.API.Graph.Request (GraphRequest, location)


data Request a = Request
    { _requestId :: UUID
    , _guiID     :: Maybe UUID
    , _request   :: a
    } deriving (Eq, Generic, Show)

makeLenses ''Request
instance Binary a => Binary (Request a)
instance ToJSON a => ToJSON (Request a)

instance GraphRequest a => GraphRequest (Request a) where
    location = request . location
