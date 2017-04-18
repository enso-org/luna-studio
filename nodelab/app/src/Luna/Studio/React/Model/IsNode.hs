{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
module Luna.Studio.React.Model.IsNode
( module Luna.Studio.React.Model.IsNode
, module X
) where

import           Control.Arrow                ((&&&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Empire.API.Data.Node         (NodeId)
import           Empire.API.Data.NodeLoc      as X (HasNodeLoc (..), nodeLoc)
import qualified Empire.API.Data.NodeLoc      as NodeLoc
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port (AnyPort, AnyPortId (InPortId', OutPortId'), InPort, InPortId, OutPort, OutPortId)
import qualified Luna.Studio.React.Model.Port as Port


type IsNode node = (HasNodeLoc node, HasPorts node)

nodeId :: HasNodeLoc node => Lens' node NodeId
nodeId = nodeLoc . NodeLoc.nodeId

toNodesMap :: HasNodeLoc node => [node] -> HashMap NodeId node
toNodesMap = HashMap.fromList . map (view nodeId &&& id)

class LookupPort portRef port | portRef -> port where
    lookupPort :: HasPorts node => node -> portRef -> Maybe port

instance LookupPort InPortId InPort where
    lookupPort node portId = node ^? inPortAt portId

instance LookupPort OutPortId OutPort where
    lookupPort node portId = node ^? outPortAt portId

instance LookupPort AnyPortId AnyPort where
    lookupPort node (InPortId'  portId) = InPortId' `fmap2` lookupPort node portId
    lookupPort node (OutPortId' portId) = OutPortId' `fmap2` lookupPort node portId

class HasPorts node where
    inPortsList :: node -> [InPort]
    outPortsList :: node -> [OutPort]
    inPortAt             :: InPortId  -> Traversal' node InPort
    outPortAt            :: OutPortId -> Traversal' node OutPort
    portsList :: node -> [AnyPort]
    portsList node = (convert <$> inPortsList node) <> (convert <$> outPortsList node)
    countInPorts         :: node -> Int
    countInPorts = length . inPortsList
    countOutPorts        :: node -> Int
    countOutPorts = length . outPortsList
    countArgPorts           :: node -> Int
    countArgPorts = length .  filter (Port.isArg . view Port.portId) . inPortsList
    countProjectionPorts :: node -> Int
    countProjectionPorts = length .  filter (Port.isProjection . view Port.portId) . outPortsList

class HasPort portId where
    hasPort :: HasPorts node =>  portId -> node -> Bool
instance HasPort AnyPortId where
    hasPort (InPortId' portId) = hasPort portId
    hasPort (OutPortId' portId) = hasPort portId
instance HasPort InPortId where
    hasPort portId node = isJust $ node ^? inPortAt  portId
instance HasPort OutPortId where
    hasPort portId node = isJust $ node ^? outPortAt portId
