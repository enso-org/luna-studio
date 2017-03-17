module Luna.Studio.React.Model.Connection
  ( module Luna.Studio.React.Model.Connection
  , ConnectionId
  ) where

import           Data.Aeson                 (ToJSON)
import           Data.Convert               (Convertible (convert))
import           Data.Position              (Position)

import           Empire.API.Data.Connection (ConnectionId)
import qualified Empire.API.Data.Connection as API
import           Empire.API.Data.Node       (NodeId)
import           Empire.API.Data.Port       (InPort, OutPort)
import           Empire.API.Data.PortRef    (AnyPortRef, InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef    as PortRef
import           Luna.Studio.Data.Color     (Color)
import           Luna.Studio.Prelude        hiding (from, set, to)


data Connection = Connection { _api    :: API.Connection
                             , _srcPos :: Position
                             , _dstPos :: Position
                             , _color  :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

src :: Lens' Connection OutPortRef
src = api . API.src

dst :: Lens' Connection InPortRef
dst = api . API.dst

connectionId :: Lens' Connection ConnectionId
connectionId = api . API.connectionId

srcNodeId :: Lens' Connection NodeId
srcNodeId = api . API.srcNodeId

srcPortId :: Lens' Connection OutPort
srcPortId = api . API.srcPortId

dstNodeId :: Lens' Connection NodeId
dstNodeId = api . API.dstNodeId

dstPortId :: Lens' Connection InPort
dstPortId = api . API.dstPortId

containsNode :: NodeId -> Connection -> Bool
containsNode nid = API.containsNode nid . convert

containsPortRef :: AnyPortRef -> Connection -> Bool
containsPortRef ref = API.containsPortRef ref . convert

data CurrentConnection = CurrentConnection { _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection

instance Convertible Connection CurrentConnection where
    convert = CurrentConnection <$> view srcPos <*> view dstPos <*> view color

toConnection :: API.Connection -> CurrentConnection -> Connection
toConnection api' = Connection api' <$> view currentFrom <*> view currentTo <*> view currentColor

instance Convertible Connection API.Connection where
    convert = view api
