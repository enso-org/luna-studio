module Luna.Studio.React.Model.Connection where

import           Control.Arrow                ((&&&))
import           Data.Aeson                   (ToJSON)
import           Data.Convert                 (Convertible (convert))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.Position                (Position)
import qualified Empire.API.Data.Connection   as Empire
import           Empire.API.Data.PortRef      (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Data.Color       (Color)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (NodeLoc)
import           Luna.Studio.React.Model.Port (InPortId, OutPortId)


type ConnectionId = InPortRef
data Mode = Normal | Sidebar | Highlighted | Dimmed deriving (Eq, Show, Typeable, Generic)

instance ToJSON Mode

data Connection = Connection { _src    :: OutPortRef
                             , _dst    :: InPortRef
                             , _srcPos :: Position
                             , _dstPos :: Position
                             , _mode   :: Mode
                             , _color  :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

data CurrentConnection = CurrentConnection { _currentFrom  :: Position
                                           , _currentTo    :: Position
                                           , _currentMode  :: Mode
                                           , _currentColor :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection


type ConnectionsMap = HashMap ConnectionId Connection

toConnectionsMap :: [Connection] -> ConnectionsMap
toConnectionsMap = HashMap.fromList . map (view connectionId &&& id)


connectionId :: Lens' Connection ConnectionId
connectionId = dst

srcNodeLoc :: Lens' Connection NodeLoc
srcNodeLoc = src . PortRef.srcNodeLoc

srcPortId :: Lens' Connection OutPortId
srcPortId = src . PortRef.srcPortId

dstNodeLoc :: Lens' Connection NodeLoc
dstNodeLoc = dst . PortRef.dstNodeLoc

dstPortId :: Lens' Connection InPortId
dstPortId = dst . PortRef.dstPortId

raw :: Getter Connection (OutPortRef, InPortRef)
raw = to raw' where
    raw' conn = (conn ^. src, conn ^. dst)

nodeLocs :: Getter Connection (NodeLoc, NodeLoc)
nodeLocs = to nodeLocs' where
    nodeLocs' conn = ( conn ^. src . PortRef.srcNodeLoc
                    , conn ^. dst . PortRef.dstNodeLoc )

containsNode :: NodeLoc -> Connection -> Bool
containsNode nid conn = (conn ^. srcNodeLoc == nid)
                     || (conn ^. dstNodeLoc == nid)

containsPortRef :: AnyPortRef -> Connection -> Bool
containsPortRef (InPortRef'  inPortRef)  conn = conn ^. dst == inPortRef
containsPortRef (OutPortRef' outPortRef) conn = conn ^. src == outPortRef

toValidEmpireConnection :: AnyPortRef -> AnyPortRef -> Maybe Empire.Connection
toValidEmpireConnection (OutPortRef' src') (InPortRef' dst')     =
    if src' ^. PortRef.srcNodeLoc /= dst' ^. PortRef.dstNodeLoc
    then Just $ Empire.Connection src' dst'
    else Nothing
toValidEmpireConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidEmpireConnection src' dst'
toValidEmpireConnection _ _                                      = Nothing


instance Convertible Connection CurrentConnection where
    convert = CurrentConnection <$> view srcPos <*> view dstPos <*> view mode <*> view color

toConnection :: OutPortRef -> InPortRef -> CurrentConnection -> Connection
toConnection src' dst' = Connection src' dst' <$> view currentFrom <*> view currentTo <*> view currentMode <*> view currentColor

instance Convertible Connection Empire.Connection where
    convert = Empire.Connection <$> view src <*> view dst
