{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
module NodeEditor.React.Model.IsNode
( module NodeEditor.React.Model.IsNode
, module X
) where

import           Common.Prelude
import           Control.Arrow               ((&&&))
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           LunaStudio.Data.NodeId      (NodeId)
import           LunaStudio.Data.NodeLoc     as X (HasNodeLoc (..), nodeLoc)
import qualified LunaStudio.Data.NodeLoc     as NodeLoc
import           LunaStudio.Data.PortRef     (InPortRef (..))
import           NodeEditor.React.Model.Port (AnyPort, AnyPortId (InPortId', OutPortId'), InPort, InPortId, InPortIndex (Arg), OutPort,
                                              OutPortId)
import qualified NodeEditor.React.Model.Port as Port


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
    lookupPort node (InPortId'  portId)
        = InPortId' `fmap2` lookupPort node portId
    lookupPort node (OutPortId' portId)
        = OutPortId' `fmap2` lookupPort node portId

class HasNodeLoc node => HasPorts node where
    inPortsList  :: node -> [InPort]
    outPortsList :: node -> [OutPort]
    inPortAt             :: InPortId  -> Traversal' node InPort
    outPortAt            :: OutPortId -> Traversal' node OutPort
    portModeAt           :: AnyPortId -> Traversal' node Port.Mode
    portsList :: node -> [AnyPort]
    portsList node
        = (convert <$> inPortsList node) <> (convert <$> outPortsList node)
    countInPorts :: node -> Int
    countInPorts = length . inPortsList
    countOutPorts :: node -> Int
    countOutPorts = length . outPortsList
    countArgPorts :: node -> Int
    countArgPorts
        = length . filter (Port.isArg . view Port.portId) . inPortsList
    countProjectionPorts :: node -> Int
    countProjectionPorts
        = length . filter (Port.isProjection . view Port.portId) . outPortsList
    argumentConstructorRef :: node -> InPortRef
    argumentConstructorRef n = InPortRef (n ^. nodeLoc) [Arg $ countArgPorts n]

class HasPort portId where
    hasPort :: HasPorts node =>  portId -> node -> Bool
instance HasPort AnyPortId where
    hasPort (InPortId' portId) = hasPort portId
    hasPort (OutPortId' portId) = hasPort portId
instance HasPort InPortId where
    hasPort portId node = isJust $ node ^? inPortAt  portId
instance HasPort OutPortId where
    hasPort portId node = isJust $ node ^? outPortAt portId
