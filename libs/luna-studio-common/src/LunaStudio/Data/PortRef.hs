module LunaStudio.Data.PortRef
    ( module LunaStudio.Data.PortRef
    , module X
    ) where

import LunaStudio.Data.NodeLoc as X (nodeLoc)

import Prologue

import qualified LunaStudio.Data.NodeLoc as NodeLoc

import Control.DeepSeq         (NFData)
import Control.Lens            (makePrisms)
import Data.Aeson.Types        (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary             (Binary)
import LunaStudio.Data.Node    (NodeId)
import LunaStudio.Data.NodeLoc (HasNodeLoc (..), NodeLoc)
import LunaStudio.Data.Port    (AnyPortId (..), InPortId, OutPortId,
                                OutPortIndex (Projection))



data InPortRef = InPortRef
    { _dstNodeLoc :: NodeLoc
    , _dstPortId  :: InPortId
    } deriving (Eq, Generic, Ord, Show)

data OutPortRef = OutPortRef
    { _srcNodeLoc :: NodeLoc
    , _srcPortId  :: OutPortId
    } deriving (Eq, Generic, Ord, Show)

data AnyPortRef
    = OutPortRef' OutPortRef
    | InPortRef'  InPortRef
    deriving (Eq, Generic, Show)

makeLenses ''AnyPortRef
makePrisms ''AnyPortRef
makeLenses ''OutPortRef
makePrisms ''OutPortRef
makeLenses ''InPortRef
makePrisms ''InPortRef

instance Binary      AnyPortRef
instance NFData      AnyPortRef
instance FromJSON    AnyPortRef
instance FromJSONKey AnyPortRef
instance ToJSON      AnyPortRef
instance ToJSONKey   AnyPortRef
instance Binary      InPortRef
instance NFData      InPortRef
instance FromJSON    InPortRef
instance FromJSONKey InPortRef
instance ToJSON      InPortRef
instance ToJSONKey   InPortRef
instance Binary      OutPortRef
instance NFData      OutPortRef
instance FromJSON    OutPortRef
instance ToJSON      OutPortRef


instance Ord AnyPortRef where
    (InPortRef'  _)  `compare` (OutPortRef' _) = LT
    (OutPortRef' _)  `compare` (InPortRef'  _) = GT
    (InPortRef'  a)  `compare` (InPortRef'  b) = a `compare` b
    (OutPortRef' a)  `compare` (OutPortRef' b) = a `compare` b

instance HasNodeLoc InPortRef  where nodeLoc = dstNodeLoc
instance HasNodeLoc OutPortRef where nodeLoc = srcNodeLoc
instance HasNodeLoc AnyPortRef where
    nodeLoc = lens getNodeLoc setNodeLoc where
        getNodeLoc (OutPortRef' outPortRef) = outPortRef ^. nodeLoc
        getNodeLoc (InPortRef'  inPortRef)  = inPortRef  ^. nodeLoc
        setNodeLoc (OutPortRef' outPortRef) nl
            = OutPortRef' $ outPortRef & nodeLoc .~ nl
        setNodeLoc (InPortRef'  inPortRef ) nl
            = InPortRef'  $ inPortRef  & nodeLoc .~ nl

class    PortId a         where toAnyPortRef :: NodeLoc -> a -> AnyPortRef
instance PortId InPortId  where toAnyPortRef = InPortRef'  .: InPortRef
instance PortId OutPortId where toAnyPortRef = OutPortRef' .: OutPortRef
instance PortId AnyPortId where
    toAnyPortRef nl (InPortId'  pid) = toAnyPortRef nl pid
    toAnyPortRef nl (OutPortId' pid) = toAnyPortRef nl pid

{-# DEPRECATED nodeId "Use nodeLoc" #-}
nodeId :: Lens' AnyPortRef NodeId
nodeId = nodeLoc . NodeLoc.nodeId

portId :: Lens' AnyPortRef AnyPortId
portId f (OutPortRef' (OutPortRef nl pid))
    = OutPortRef' . OutPortRef nl . outPortId' <$> f (OutPortId' pid)
portId f (InPortRef'  (InPortRef  nl pid))
    = InPortRef'  . InPortRef  nl . inPortId'  <$> f (InPortId'  pid)

dstNodeId :: Lens' InPortRef NodeId
dstNodeId = dstNodeLoc . NodeLoc.nodeId

srcNodeId :: Lens' OutPortRef NodeId
srcNodeId = srcNodeLoc . NodeLoc.nodeId
