module Luna.Studio.State.Collaboration where

import           Data.Aeson                           (ToJSON)
import           Data.DateTime                        (DateTime)
import           Data.Map.Lazy                        (Map)
import           Luna.Studio.Prelude

import           Empire.API.Graph.CollaborationUpdate (ClientId)


newtype ColorId = ColorId { unColorId :: Int } deriving (Eq, Show, Generic, NFData)

numColors :: Int
numColors = 8

data Client = Client { _lastSeen :: DateTime
                     , _colorId  :: ColorId
                     } deriving (Eq, Show, Generic)

newtype State = State { _knownClients :: Map ClientId Client
                      } deriving (Default, Eq, Generic, Show)

makeLenses ''State
makeLenses ''Client

instance ToJSON State
instance ToJSON Client
instance ToJSON ColorId
