module Luna.Studio.React.Model.Connection where

import           Data.Aeson                 (ToJSON)
import           Data.Position              (Position)
import           Empire.API.Data.Connection (ConnectionId)
import           Luna.Studio.Data.Color     (Color)
import           Luna.Studio.Prelude        hiding (from, set, to)



data Connection = Connection { _connectionId :: ConnectionId
                             , _from         :: Position
                             , _to           :: Position
                             , _color        :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

data CurrentConnection = CurrentConnection { _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection

toCurrentConnection :: Connection -> CurrentConnection
toCurrentConnection = CurrentConnection <$> view from <*> view to <*> view color

toConnection :: ConnectionId -> CurrentConnection -> Connection
toConnection connId = Connection connId <$> view currentFrom <*> view currentTo <*> view currentColor
