module Luna.Studio.React.Model.Connection
  ( module Luna.Studio.React.Model.Connection
  , ConnectionId
  ) where

import           Data.Aeson                 (ToJSON)
import           Data.Position              (Position)
import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.PortRef    (InPortRef, OutPortRef)
import           Luna.Studio.Data.Color     (Color)
import           Luna.Studio.Prelude        hiding (from, set, to)



data Connection = Connection { _src   :: OutPortRef
                             , _dst   :: InPortRef
                             , _from  :: Position
                             , _to    :: Position
                             , _color :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

connectionId :: Lens' Connection ConnectionId
connectionId = dst

data CurrentConnection = CurrentConnection { _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection

toCurrentConnection :: Connection -> CurrentConnection
toCurrentConnection = CurrentConnection <$> view from <*> view to <*> view color

toConnection :: OutPortRef -> InPortRef -> CurrentConnection -> Connection
toConnection src' dst' = Connection src' dst' <$> view currentFrom <*> view currentTo <*> view currentColor
