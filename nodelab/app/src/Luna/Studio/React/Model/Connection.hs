module Luna.Studio.React.Model.Connection
  ( module Luna.Studio.React.Model.Connection
  , ConnectionId
  ) where

import           Control.Arrow                ((&&&))
import           Data.Aeson                   (ToJSON)
import           Data.Convert                 (Convertible (convert))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Position                (Position)
import           Empire.API.Data.Connection   (ConnectionId)
import qualified Empire.API.Data.Connection   as Empire
import           Empire.API.Data.PortRef      (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Data.Color       (Color)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (NodeId)
import           Luna.Studio.React.Model.Port (InPort, OutPort)



data Connection = Connection { _src    :: OutPortRef
                             , _dst    :: InPortRef
                             , _srcPos :: Position
                             , _dstPos :: Position
                             , _color  :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

data CurrentConnection = CurrentConnection { _currentFrom         :: Position
                                           , _currentTo           :: Position
                                           , _currentColor        :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection


type ConnectionsMap = HashMap ConnectionId Connection

toConnectionsMap :: [Connection] -> ConnectionsMap
toConnectionsMap = HashMap.fromList . map (view connectionId &&& id)


connectionId :: Lens' Connection ConnectionId
connectionId = dst

srcNodeId :: Lens' Connection NodeId
srcNodeId = src . PortRef.srcNodeId

srcPortId :: Lens' Connection OutPort
srcPortId = src . PortRef.srcPortId

dstNodeId :: Lens' Connection NodeId
dstNodeId = dst . PortRef.dstNodeId

dstPortId :: Lens' Connection InPort
dstPortId = dst . PortRef.dstPortId

raw :: Getter Connection (OutPortRef, InPortRef)
raw = to raw' where
    raw' conn = (conn ^. src, conn ^. dst)

nodeIds :: Getter Connection (NodeId, NodeId)
nodeIds = to nodeIds' where
    nodeIds' conn = ( conn ^. src . PortRef.srcNodeId
                    , conn ^. dst . PortRef.dstNodeId )

containsNode :: NodeId -> Connection -> Bool
containsNode nid conn = (conn ^. srcNodeId == nid)
                     || (conn ^. dstNodeId == nid)

containsPortRef :: AnyPortRef -> Connection -> Bool
containsPortRef (InPortRef'  inPortRef)  conn = conn ^. dst == inPortRef
containsPortRef (OutPortRef' outPortRef) conn = conn ^. src == outPortRef

toValidEmpireConnection :: AnyPortRef -> AnyPortRef -> Maybe Empire.Connection
toValidEmpireConnection (OutPortRef' src') (InPortRef' dst')     =
    if src' ^. PortRef.srcNodeId /= dst' ^. PortRef.dstNodeId
    then Just $ Empire.Connection src' dst'
    else Nothing
toValidEmpireConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidEmpireConnection src' dst'
toValidEmpireConnection _ _                                      = Nothing


instance Convertible Connection CurrentConnection where
    convert = CurrentConnection <$> view srcPos <*> view dstPos <*> view color

toConnection :: OutPortRef -> InPortRef -> CurrentConnection -> Connection
toConnection src' dst' = Connection src' dst' <$> view currentFrom <*> view currentTo <*> view currentColor

instance Convertible Connection Empire.Connection where
    convert conn = Empire.Connection
        {- src -} (conn ^. src)
        {- dst -} (conn ^. dst)
