module Empire.ApiHandlers where

import Prologue

import qualified Data.Map                                as Map
import qualified Data.Set                         as Set
import qualified Empire.Commands.Graph            as Graph
import qualified Empire.Data.Graph                       as Graph (code,
                                                                   nodeCache)
import qualified LunaStudio.API.Graph.AddConnection      as AddConnection
import qualified LunaStudio.API.Graph.AddImports         as AddImports
import qualified LunaStudio.API.Graph.AddNode            as AddNode
import qualified LunaStudio.API.Graph.AddPort            as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph        as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes    as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollapseToFunction as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy               as Copy
import qualified LunaStudio.API.Graph.DumpGraphViz       as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram         as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs       as GetSubgraphs
import qualified LunaStudio.API.Graph.MovePort           as MovePort
import qualified LunaStudio.API.Graph.Paste              as Paste
import qualified LunaStudio.API.Graph.RemoveConnection   as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes        as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort         as RemovePort
import qualified LunaStudio.API.Graph.RenameNode         as RenameNode
import qualified LunaStudio.API.Graph.RenamePort         as RenamePort
import qualified LunaStudio.API.Graph.Request            as G
import qualified LunaStudio.API.Graph.SaveSettings       as SaveSettings
import qualified LunaStudio.API.Graph.SearchNodes        as SearchNodes
import qualified LunaStudio.API.Graph.SetCode            as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck          as TypeCheck
import qualified LunaStudio.Data.Breadcrumb              as Breadcrumb
import qualified LunaStudio.Data.Connection       as Connection
import qualified LunaStudio.Data.GraphLocation           as GraphLocation
import qualified LunaStudio.Data.Node             as Node
import qualified LunaStudio.Data.NodeLoc                 as NodeLoc
import qualified LunaStudio.Data.PortRef          as PortRef

import Control.Lens                  (to, traversed, use, (.=), (^..))
import Empire.ASTOp                  (runASTOp)
import Empire.Commands.GraphBuilder  (buildClassGraph, buildConnections,
                                      buildGraph, buildNodes, getNodeCode,
                                      getNodeName)
import LunaStudio.Data.GraphLocation (GraphLocation (..))
import LunaStudio.Data.NodeLoc       (NodeLoc (..))
import LunaStudio.Data.PortRef       (InPortRef (..), OutPortRef (..), AnyPortRef (..))
import LunaStudio.Data.Node          (ExpressionNode (..), NodeId)
import LunaStudio.Data.Port          (InPort (..), InPortIndex (..),
                                      OutPort (..), OutPortIndex (..),
                                      Port (..), PortState (..), getPortNumber)

import Empire.Empire         (Empire)
import LunaStudio.Data.Graph (Graph (Graph))

type family InverseOf a
type family ResultOf  a

-- | The law satisfied by all instances of this class should be:
--   buildInverse a >>= \inv -> perform a >> perform inv ~ pure ()
--   Where a ~ b denotes equality in the sense of `getProgram` after
--   performing a and b.
class Modification a where
    perform      :: a -> Empire (ResultOf a)

    buildInverse :: a -> Empire (InverseOf a)
    default buildInverse :: (InverseOf a ~ ()) => a -> Empire (InverseOf a)
    buildInverse = const $ pure ()

type instance InverseOf () = ()
type instance ResultOf  () = ()
instance Modification () where
    perform       _ = pure ()

type instance InverseOf    AddNode.Request = ()
type instance ResultOf     AddNode.Request = ()
instance      Modification AddNode.Request where
    perform (AddNode.Request loc nl expression nodeMeta connectTo) =
        void $ Graph.addNodeWithConnection loc nl expression nodeMeta connectTo

type instance InverseOf    RemoveNodes.Request = RemoveNodes.Inverse
type instance ResultOf     RemoveNodes.Request = ()
instance      Modification RemoveNodes.Request where
    perform (RemoveNodes.Request location nodeLocs) =
        Graph.removeNodes location $ convert <$> nodeLocs
    buildInverse (RemoveNodes.Request location nodeLocs) = do
        let nodeIds = convert <$> nodeLocs
        Graph allNodes allConnections _ _ monads _ <- Graph.getGraph location
        let isNodeRelevant n = Set.member (n ^. Node.nodeId) idSet
            isConnRelevant c
                =  Set.member (c ^. Connection.src . PortRef.srcNodeId) idSet
                || Set.member (c ^. Connection.dst . PortRef.dstNodeId) idSet
            idSet = Set.fromList nodeIds
            nodes = filter isNodeRelevant allNodes
            conns = filter isConnRelevant allConnections
        pure $ RemoveNodes.Inverse nodes conns

getSrcPortByNodeId :: NodeId -> OutPortRef
getSrcPortByNodeId nid = OutPortRef (NodeLoc def nid) []

getDstPortByNodeLoc :: NodeLoc -> AnyPortRef
getDstPortByNodeLoc nl = InPortRef' $ InPortRef nl [Self]


type instance InverseOf    AddConnection.Request = AddConnection.Inverse
type instance ResultOf     AddConnection.Request = ()
instance      Modification AddConnection.Request where
    perform (AddConnection.Request location src' dst') = do
        let getSrcPort = either id getSrcPortByNodeId
            getDstPort = either id getDstPortByNodeLoc
        void $ Graph.connectCondTC True location (getSrcPort src')
                                                 (getDstPort dst')
    buildInverse (AddConnection.Request location _ dst') = do
        let dstNodeId = either (view PortRef.nodeId) (view NodeLoc.nodeId) dst'
        prevExpr <- Graph.withGraph location . runASTOp $ getNodeCode dstNodeId
        pure $ AddConnection.Inverse prevExpr

type instance InverseOf    RenamePort.Request = RenamePort.Inverse
type instance ResultOf     RenamePort.Request = ()
instance      Modification RenamePort.Request where
    perform (RenamePort.Request location portRef name)
        = Graph.renamePort location portRef name
    buildInverse (RenamePort.Request location portRef name) = do
        oldName <- Graph.getPortName location portRef
        pure $ RenamePort.Inverse oldName

type instance InverseOf    RenameNode.Request = RenameNode.Inverse
type instance ResultOf     RenameNode.Request = ()
instance      Modification RenameNode.Request where
    perform (RenameNode.Request location nodeId name)
        = Graph.renameNode location nodeId name
    buildInverse (RenameNode.Request location nodeId name) = do
        prevName <- Graph.getName location nodeId
        pure $ RenameNode.Inverse $ fromMaybe "" prevName

type instance InverseOf    SetPortDefault.Request = SetPortDefault.Inverse
type instance ResultOf     SetPortDefault.Request = ()
instance      Modification SetPortDefault.Request where
    perform (SetPortDefault.Request location portRef defaultValue)
        = Graph.setPortDefault location portRef defaultValue
    buildInverse (SetPortDefault.Request location portRef _)
        = SetPortDefault.Inverse <$> Graph.getPortDefault location portRef

type instance InverseOf    AddImports.Request = ()
type instance ResultOf     AddImports.Request = ()
instance      Modification AddImports.Request where
    perform (AddImports.Request location modules)
        = Graph.addImports location modules

type instance InverseOf    AddPort.Request = ()
type instance ResultOf     AddPort.Request = ()
instance      Modification AddPort.Request where
    perform (AddPort.Request location portRef connsDst name)
        = Graph.addPortWithConnections location portRef name connsDst

type instance InverseOf    AddSubgraph.Request = ()
type instance ResultOf     AddSubgraph.Request = ()
instance      Modification AddSubgraph.Request where
    perform (AddSubgraph.Request location nodes connections)
        = void $ Graph.addSubgraph location nodes connections

type instance InverseOf    AutolayoutNodes.Request = AutolayoutNodes.Inverse
type instance ResultOf     AutolayoutNodes.Request = ()
instance      Modification AutolayoutNodes.Request where
    perform (AutolayoutNodes.Request location nodeLocs _)
        = Graph.autolayoutNodes location (convert <$> nodeLocs)
    buildInverse (AutolayoutNodes.Request location nodeLocs _) = do
        positions <- Graph.getNodeMetas location nodeLocs
        pure $ AutolayoutNodes.Inverse $ catMaybes positions

type instance InverseOf    CollapseToFunction.Request = CollapseToFunction.Inverse
type instance ResultOf     CollapseToFunction.Request = ()
instance      Modification CollapseToFunction.Request where
    perform (CollapseToFunction.Request location locs) = do
        let ids = convert <$> locs
        Graph.collapseToFunction location ids
    buildInverse (CollapseToFunction.Request (GraphLocation file _) _) = do
        code <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        cache <- Graph.prepareNodeCache (GraphLocation file def)
        pure $ CollapseToFunction.Inverse code cache

type instance InverseOf    Copy.Request = ()
type instance ResultOf     Copy.Request = Copy.Result
instance      Modification Copy.Request where
    perform (Copy.Request location nodeLocs) = do
        r <- Graph.prepareCopy location (convert nodeLocs)
        pure $ Copy.Result r r --FIXME

type instance InverseOf    DumpGraphViz.Request = ()
type instance ResultOf     DumpGraphViz.Request = ()
instance      Modification DumpGraphViz.Request where
    perform (DumpGraphViz.Request location)
        = Graph.dumpGraphViz location

type instance InverseOf    GetSubgraphs.Request = ()
type instance ResultOf     GetSubgraphs.Request = GetSubgraphs.Result
instance      Modification GetSubgraphs.Request where
    perform (GetSubgraphs.Request location) = do
        graph <- Graph.getGraph location
        let bc = location ^.
                GraphLocation.breadcrumb . Breadcrumb.items . to unsafeLast
        pure . GetSubgraphs.Result $ Map.singleton bc graph --FIXME: should return multiple graphs

type instance InverseOf    MovePort.Request = ()
type instance ResultOf     MovePort.Request = ()
instance      Modification MovePort.Request where
    perform (MovePort.Request location portRef newPortPos)
        = Graph.movePort location portRef newPortPos

type instance InverseOf    Paste.Request = ()
type instance ResultOf     Paste.Request = ()
instance      Modification Paste.Request where
    perform (Paste.Request location position string)
        = Graph.paste location position string

data ConnectionDoesNotExistException
    = ConnectionDoesNotExistException InPortRef
    deriving (Show)

instance Exception ConnectionDoesNotExistException where
    fromException = astExceptionFromException
    toException   = astExceptionToException

data DestinationDoesNotExistException
    = DestinationDoesNotExistException InPortRef
    deriving (Show)

instance Exception DestinationDoesNotExistException where
    fromException = astExceptionFromException
    toException   = astExceptionToException

type instance InverseOf    RemoveConnection.Request = RemoveConnection.Inverse
type instance ResultOf     RemoveConnection.Request = ()
instance      Modification RemoveConnection.Request where
    perform (RemoveConnection.Request location dst) do
        mayDstNode <- getNodeById location $ dst ^. PortRef.dstNodeId
        when (isNothing mayDstNode)
            $ throwM $ DestinationDoesNotExistException dst
        Graph.disconnect location dst
    buildInverse (RemoveConnection.Request location dst) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        case find (\conn -> snd conn == dst) connections of
            Nothing       -> throwM $ ConnectionDoesNotExistException dst
            Just (src, _) -> pure $ RemoveConnection.Inverse src

data SidebarDoesNotExistException = SidebarDoesNotExistException
    deriving (Show)

instance Exception SidebarDoesNotExistException where
    fromException = astExceptionFromException
    toException = astExceptionToException

type instance InverseOf    RemovePort.Request = RemovePort.Inverse
type instance ResultOf     RemovePort.Request = ()
instance      Modification RemovePort.Request where
    buildInverse (RemovePort.Request location portRef) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        oldName     <- Graph.getPortName location portRef
        let conns = flip filter connections $ (== portRef) . fst
        pure $ RemovePort.Inverse oldName $ fmap (uncurry Connection) conns
    perform (RemovePort.Request location portRef) = do
        maySidebar <- view GraphAPI.inputSidebar <$> Graph.getGraphNoTC location
        when (isNothing maySidebar) $ throwM SidebarDoesNotExistException
        Graph.removePort location portRef

type instance InverseOf    SetCode.Request = SetCode.Inverse
type instance ResultOf     SetCode.Request = ()
instance      Modification SetCode.Request where
    buildInverse (SetCode.Request location@(GraphLocation file _) _ _) = do
        cache <- Graph.prepareNodeCache location
        code  <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        pure $ SetCode.Inverse code cache
    perform (SetCode.Request location@(GraphLocation file _) code cache) = do
        Graph.withUnit (GraphLocation file def) $ Graph.nodeCache .= cache
        Graph.loadCode location code
        Graph.resendCode location

type instance InverseOf    SetCode.Request = SetCode.Inverse
type instance ResultOf     SetCode.Request = ()
instance      Modification SetCode.Request where
    buildInverse (SetCode.Request location@(GraphLocation file _) _ _) = do
        cache <- Graph.prepareNodeCache location
        code  <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        pure $ SetCode.Inverse code cache
    perform (SetCode.Request location@(GraphLocation file _) code cache) = do
        Graph.withUnit (GraphLocation file def) $ Graph.nodeCache .= cache
        Graph.loadCode location code
        Graph.resendCode location
