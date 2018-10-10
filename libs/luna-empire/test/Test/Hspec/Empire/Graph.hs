module Test.Hspec.Empire.Graph where

import Empire.Prelude

import qualified Data.Map                        as Map
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph               as Graph
import qualified LunaStudio.Data.Node            as Node
import qualified LunaStudio.Data.Position        as Position

import Control.Lens                  (uses)
import Data.UUID                     (UUID)
import Data.UUID.V4                  (nextRandom)
import Empire.ASTOp                  (runASTOp)
import Empire.Empire                 (Empire)
import LunaStudio.Data.Connection    (Connection)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.Node          (ExpressionNode, NodeId)
import LunaStudio.Data.NodeLoc       (NodeLoc (NodeLoc))
import LunaStudio.Data.NodeMeta      (NodeMeta (NodeMeta))
import LunaStudio.Data.Port          (InPort, InPortId, InPortIndex (Self),
                                      OutPort, OutPortId, Port (Port),
                                      PortState)
import LunaStudio.Data.PortRef       (AnyPortRef (InPortRef'),
                                      InPortRef (InPortRef),
                                      OutPortRef (OutPortRef))
import LunaStudio.Data.TypeRep       (TypeRep (TStar))


mkUUID :: MonadIO m => m UUID
mkUUID = liftIO nextRandom

mkAllPort :: Text -> PortState -> OutPort
mkAllPort name = Port mempty name TStar

mkSelfPort :: PortState -> InPort
mkSelfPort = Port [Self] "self" TStar

mkAliasPort :: PortState -> InPort
mkAliasPort = Port mempty "alias" TStar

outPortRef :: NodeId -> OutPortId -> OutPortRef
outPortRef = OutPortRef . NodeLoc def

inPortRef :: NodeId -> InPortId -> InPortRef
inPortRef = InPortRef . NodeLoc def


addNode :: GraphLocation -> Text -> Empire ExpressionNode
addNode gl code = mkUUID >>= \nid -> Graph.addNode gl nid code def

connectToInput :: GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connectToInput loc outPort inPort = Graph.connect loc outPort (InPortRef' inPort)

findNodeIdByName :: GraphLocation -> Text -> Empire (Maybe NodeId)
findNodeIdByName = fmap2 (view Node.nodeId) .: findNodeByName

findNodeByName :: GraphLocation -> Text -> Empire (Maybe ExpressionNode)
findNodeByName gl name = findNode <$> Graph.getNodes gl where
    filterNodes = filter (\n -> n ^. Node.name == Just name)
    findNode nodes = case filterNodes nodes of
        [n] -> Just n
        _   -> Nothing


mockNodesLayout :: GraphLocation -> Empire ()
mockNodesLayout gl = getMarkersWithNodeId >>= mapM_ (uncurry mockNodeMeta) where
    nodesInterspace          = 10
    mockMarkerOfffset marker = fromIntegral marker * nodesInterspace
    getMarkersWithNodeId = Graph.withGraph gl $ do
        markers <- fmap fromIntegral . Map.keys
            <$> use (Graph.userState . Graph.codeMarkers)
        foundNodeIds <- runASTOp . fmap catMaybes . forM markers $ \m ->
            (m,) `fmap2` Graph.getNodeIdForMarker m
        topLevelNodeIds <- uses
            (Graph.userState . Graph.breadcrumbHierarchy)
            BH.topLevelIDs
        pure $ filter (\(_, nid) -> elem nid topLevelNodeIds) foundNodeIds
    mockNodeMeta marker nid = Graph.setNodeMeta gl nid $ NodeMeta
        (Position.fromTuple (0, mockMarkerOfffset marker))
        False
        mempty

