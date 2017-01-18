module Luna.Studio.React.Model.Connection where

import           Data.Aeson                 (ToJSON)
import           Empire.API.Data.Connection (ConnectionId)
import qualified Empire.API.Data.Connection as Empire
import           Empire.API.Data.PortRef    (AnyPortRef)
import           Luna.Studio.Data.Color     (Color)
import           Luna.Studio.Data.Vector    (Position)
import           Luna.Studio.Prelude        hiding (from, set, to)



data Connection = Connection { _connectionId :: ConnectionId
                             , _from         :: Position
                             , _to           :: Position
                             , _color        :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

data CurrentConnection = CurrentConnection { _srcPortRef          :: AnyPortRef
                                           , _modifiedConnection  :: Maybe Empire.Connection
                                           , _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection
