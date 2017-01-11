module Luna.Studio.State.Collaboration where

import           Control.DeepSeq                (NFData)
import           Data.Aeson                     (ToJSON)
import           Data.DateTime                  (DateTime)
import           Data.Map.Lazy                  (Map)
import           Luna.Studio.Prelude

import           Empire.API.Graph.Collaboration (ClientId)
import           Empire.API.JSONInstances       ()

newtype ColorId = ColorId { unColorId :: Int } deriving (Eq, Show, Generic, NFData)

numColors :: Int
numColors = 8

data Client = Client { _lastSeen :: DateTime
                     , _colorId  :: ColorId
                     } deriving (Eq, Show, Generic)

data State = State { _knownClients :: Map ClientId Client
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''Client

instance ToJSON State
instance ToJSON Client
instance ToJSON ColorId

instance Default State where
    def = State def
