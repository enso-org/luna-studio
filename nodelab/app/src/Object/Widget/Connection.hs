module Object.Widget.Connection where

import           Data.Aeson                 (ToJSON)
import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.PortRef    (AnyPortRef)
import           Luna.Studio.Data.Color     (Color)
import           Luna.Studio.Data.Vector    (Position)
import           Luna.Studio.Prelude        hiding (from, set, to)
import           Object.Widget              ()



data ConnectionHighlight = None | SrcHighlight | DstHighlight deriving (Eq, Show, Generic)

data Connection = Connection { _connectionId :: ConnectionId
                             , _visible      :: Bool
                             , _from         :: Position
                             , _to           :: Position
                             , _arrow        :: Bool
                             , _color        :: Color
                             , _highlight    :: ConnectionHighlight
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection
instance ToJSON ConnectionHighlight
instance Default ConnectionHighlight where
    def = None

--TODO[react]: Find out if this can be treated as regular connection
data CurrentConnection = CurrentConnection { _srcPortRef          :: AnyPortRef
                                           , _currentVisible      :: Bool
                                           , _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentArrow        :: Bool
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection
