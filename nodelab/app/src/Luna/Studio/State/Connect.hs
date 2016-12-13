module Luna.Studio.State.Connect where


import           Data.Aeson               (ToJSON)
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Empire.API.Data.PortRef  (AnyPortRef)
import           Empire.API.JSONInstances ()


data Connecting = Connecting
                { _sourcePortRef      :: AnyPortRef
                , _sourcePortAngleVec :: Vector2 Double
                , _sourceNodePos      :: Vector2 Double
                } deriving (Eq, Show, Generic)

data State = State { _connecting  :: Maybe Connecting
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''Connecting

instance ToJSON State
instance ToJSON Connecting

instance Default State where
    def = State def
