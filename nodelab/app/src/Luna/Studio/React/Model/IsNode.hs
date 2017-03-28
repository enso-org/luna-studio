module Luna.Studio.React.Model.IsNode where

import           Empire.API.Data.Node         (NodeId)
import           Empire.API.Data.NodeLoc      (NodeLoc)
import qualified Empire.API.Data.NodeLoc      as NodeLoc
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port (Port, PortId, PortsMap)


class IsNode a where
    nodeLoc              :: Lens' a NodeLoc
    nodeId               :: Getter a NodeId
    nodeId = nodeLoc . NodeLoc.nodeId
    ports                :: Lens' a PortsMap
    getPorts             :: a -> [Port]
    hasPort              :: PortId -> a -> Bool
    countInPorts         :: a -> Int
    countOutPorts        :: a -> Int
    countArgPorts        :: a -> Int
    countProjectionPorts :: a -> Int
