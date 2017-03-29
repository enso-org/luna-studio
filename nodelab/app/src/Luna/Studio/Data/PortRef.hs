{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Data.PortRef where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Hashable                 (Hashable)
import qualified Empire.API.Data.Connection    as Empire
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeLoc       (NodeLoc, NodePath)
import qualified Empire.API.Data.NodeLoc       as NodeLoc
import           Empire.API.Data.Port          (InPort, OutPort, PortId (..))
import qualified Empire.API.Data.PortRef       as Empire
import           Luna.Studio.Batch.Workspace   (Workspace)
import qualified Luna.Studio.Batch.Workspace   as Workspace
import           Prologue


data InPortRef  = InPortRef  { _dstNodeLoc :: NodeLoc
                             , _dstPortId :: InPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data OutPortRef = OutPortRef { _srcNodeLoc :: NodeLoc
                             , _srcPortId :: OutPort
                             } deriving (Show, Eq, Generic, Ord, NFData)

data AnyPortRef = OutPortRef' OutPortRef | InPortRef' InPortRef deriving (Show, Eq, Generic, NFData)

instance Ord AnyPortRef where
  (InPortRef'  _)  `compare` (OutPortRef' _) = LT
  (OutPortRef' _)  `compare` (InPortRef'  _) = GT
  (InPortRef'  a)  `compare` (InPortRef'  b) = a `compare` b
  (OutPortRef' a)  `compare` (OutPortRef' b) = a `compare` b

nodeLoc' :: AnyPortRef -> NodeLoc
nodeLoc' (OutPortRef' (OutPortRef nl _)) = nl
nodeLoc' (InPortRef'  (InPortRef  nl _)) = nl

nodeLoc :: Getter AnyPortRef NodeLoc
nodeLoc = to nodeLoc'

portId' :: AnyPortRef -> PortId
portId' (OutPortRef' (OutPortRef _ pid)) = OutPortId pid
portId' (InPortRef'  (InPortRef  _ pid)) = InPortId  pid

portId :: Getter AnyPortRef PortId
portId = to portId'

toAnyPortRef :: NodeLoc -> PortId -> AnyPortRef
toAnyPortRef nl (InPortId pid)  = InPortRef'  $ InPortRef  nl pid
toAnyPortRef nl (OutPortId pid) = OutPortRef' $ OutPortRef nl pid

makeLenses ''AnyPortRef
makePrisms ''AnyPortRef
makeLenses ''OutPortRef
makePrisms ''OutPortRef
makeLenses ''InPortRef
makePrisms ''InPortRef

instance FromJSON AnyPortRef
instance FromJSON InPortRef
instance FromJSON OutPortRef
instance Hashable InPortRef
instance ToJSON AnyPortRef
instance ToJSON InPortRef
instance ToJSON OutPortRef

instance Convertible InPortRef Empire.InPortRef where --FIXME this instance is only for compatibility with old API
    convert (InPortRef dstLoc dstPortId') = Empire.InPortRef (dstLoc ^. NodeLoc.nodeId) dstPortId'

instance Convertible OutPortRef Empire.OutPortRef where --FIXME this instance is only for compatibility with old API
    convert (OutPortRef srcLoc srcPortId') = Empire.OutPortRef (srcLoc ^. NodeLoc.nodeId) srcPortId'

instance Convertible AnyPortRef Empire.AnyPortRef where --FIXME this instance is only for compatibility with old API
    convert (InPortRef' inPortRef) = Empire.InPortRef' $ convert inPortRef
    convert (OutPortRef' outPortRef) = Empire.OutPortRef' $ convert outPortRef

instance Convertible (OutPortRef, InPortRef) Empire.Connection where
    convert (src', dst') = Empire.Connection (convert src') (convert dst')

instance Convertible Empire.InPortRef InPortRef where --FIXME this instance is only for compatibility with old API
    convert (Empire.InPortRef dstLoc dstPortId') = InPortRef (convert dstLoc) dstPortId'

instance Convertible Empire.OutPortRef OutPortRef where --FIXME this instance is only for compatibility with old API
    convert (Empire.OutPortRef srcLoc srcPortId') = OutPortRef (convert srcLoc) srcPortId'

instance Convertible Empire.AnyPortRef AnyPortRef where --FIXME this instance is only for compatibility with old API
    convert (Empire.InPortRef'  inPortRef ) = InPortRef'  $ convert inPortRef
    convert (Empire.OutPortRef' outPortRef) = OutPortRef' $ convert outPortRef

instance Convertible (NodePath, Empire.InPortRef) InPortRef where
    convert (nodePath, Empire.InPortRef dstLoc dstPortId') = InPortRef (convert (nodePath, dstLoc)) dstPortId'

instance Convertible (NodePath, Empire.OutPortRef) OutPortRef where
    convert (nodePath, Empire.OutPortRef srcLoc srcPortId') = OutPortRef (convert (nodePath, srcLoc)) srcPortId'

instance Convertible (NodePath, Empire.AnyPortRef) AnyPortRef where
    convert (nodePath, Empire.InPortRef'  inPortRef ) = InPortRef'  $ convert (nodePath, inPortRef)
    convert (nodePath, Empire.OutPortRef' outPortRef) = OutPortRef' $ convert (nodePath, outPortRef)

instance Convertible (GraphLocation, InPortRef) (GraphLocation, Empire.InPortRef) where
    convert (graphLoc, inPortRef) = (newGraphLoc, Empire.InPortRef dstNodId $ inPortRef ^. dstPortId) where
        (newGraphLoc, dstNodId) = convert (graphLoc, inPortRef ^. dstNodeLoc)

instance Convertible (Workspace, InPortRef) (Workspace, Empire.InPortRef) where
    convert (workspace, inPortRef) = (newWorkspace, Empire.InPortRef dstNodId $ inPortRef ^. dstPortId) where
      (newWorkspace, dstNodId) = convert (workspace, inPortRef ^. dstNodeLoc)

instance Convertible (GraphLocation, OutPortRef) (GraphLocation, Empire.OutPortRef) where
    convert (graphLoc, outPortRef) = (newGraphLoc, Empire.OutPortRef srcNodeId $ outPortRef ^. srcPortId) where
        (newGraphLoc, srcNodeId) = convert (graphLoc, outPortRef ^. srcNodeLoc)

instance Convertible (Workspace, OutPortRef) (Workspace, Empire.OutPortRef) where
    convert (workspace, outPortRef) = (newWorkspace, Empire.OutPortRef srcNodeId $ outPortRef ^. srcPortId) where
      (newWorkspace, srcNodeId) = convert (workspace, outPortRef ^. srcNodeLoc)


instance Convertible (GraphLocation, AnyPortRef) (GraphLocation, Empire.AnyPortRef) where
    convert (graphLoc, OutPortRef' outPortRef) = let (graphLoc', outPortRef') = convert (graphLoc, outPortRef) in (graphLoc', Empire.OutPortRef' outPortRef')
    convert (graphLoc, InPortRef'  inPortRef ) = let (graphLoc', inPortRef' ) = convert (graphLoc, inPortRef ) in (graphLoc', Empire.InPortRef'  inPortRef' )

instance Convertible (Workspace, AnyPortRef) (Workspace, Empire.AnyPortRef) where
    convert (workspace, anyPortRef) = (workspace & Workspace.currentLocation .~ location', anyPortRef') where
        (location', anyPortRef') = convert (workspace ^. Workspace.currentLocation, anyPortRef)
