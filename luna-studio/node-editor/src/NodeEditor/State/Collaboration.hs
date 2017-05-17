module NodeEditor.State.Collaboration where

import           Common.Prelude
import           Data.DateTime                            (DateTime)
import           Data.Map.Lazy                            (Map)
import           LunaStudio.API.Graph.CollaborationUpdate (ClientId)


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
