module Object.Widget.Connection where

import           Data.Aeson                 (ToJSON)
import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.PortRef    (AnyPortRef)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude        hiding (from, set, to)
import qualified Luna.Studio.Prelude        as Prelude
import           Object.Widget

data ConnectionHighlight = None | SrcHighlight | DstHighlight deriving (Eq, Show, Generic)

data Connection = Connection { _connectionId :: ConnectionId
                             , _visible      :: Bool
                             , _from         :: Vector2 Double
                             , _to           :: Vector2 Double
                             , _arrow        :: Bool
                             , _color        :: Int
                             , _highlight    :: ConnectionHighlight
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection
instance ToJSON ConnectionHighlight
instance Default ConnectionHighlight where
    def = None

instance IsDisplayObject Connection where
    widgetPosition = from
    widgetSize     = lens get set where
        get w      = abs <$> (w ^. from - w ^. to)
        set w s    = w & to .~ ((w ^. from) + s)
    widgetVisible  = Prelude.to $ const True

--TODO[react]: Find out if this can be treated as regular connection
data CurrentConnection = CurrentConnection { _srcPortRef          :: AnyPortRef
                                           , _currentVisible      :: Bool
                                           , _currentFrom         :: Vector2 Double
                                           , _currentTo           :: Vector2 Double
                                           , _currentArrow        :: Bool
                                           , _currentColor        :: Int
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection

instance IsDisplayObject CurrentConnection where
    widgetPosition = currentFrom
    widgetSize     = lens get set where
        get w      = abs <$> (w ^. currentFrom - w ^. currentTo)
        set w s    = w & currentTo .~ ((w ^. currentFrom) + s)
    widgetVisible  = Prelude.to $ const True
