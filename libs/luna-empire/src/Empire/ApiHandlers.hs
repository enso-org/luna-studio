module Empire.ApiHandlers where

import Prologue hiding (init, last)

import qualified Data.Map                                as Map
import qualified Data.Set                         as Set
import qualified Data.UUID.V4                            as UUID
import qualified Empire.Commands.Graph            as Graph
import qualified Empire.Data.Graph                       as Graph (code,
                                                                   nodeCache)
import qualified LunaStudio.API.Atom.Copy                as CopyText
import qualified LunaStudio.API.Atom.GetBuffer           as GetBuffer
import qualified LunaStudio.API.Atom.Paste               as PasteText
import qualified LunaStudio.API.Atom.Substitute          as Substitute
import qualified LunaStudio.API.Control.Interpreter      as Interpreter
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
import qualified LunaStudio.API.Response                 as Response
import qualified LunaStudio.Data.Breadcrumb              as Breadcrumb
import qualified LunaStudio.Data.Connection       as Connection
import qualified LunaStudio.Data.Graph                   as GraphAPI
import qualified LunaStudio.Data.GraphLocation           as GraphLocation
import qualified LunaStudio.Data.Node             as Node
import qualified LunaStudio.Data.NodeLoc                 as NodeLoc
import qualified LunaStudio.Data.PortRef          as PortRef
import qualified LunaStudio.Data.Project                 as Project
import qualified Path
import qualified System.Log.MLogger                      as Logger

import Control.Lens                  (to, traversed, use, (.=), (^..), _Left)
import Control.Monad.Catch           (handle, try)
import Data.List                     (break, find, init, last, partition, sortBy)
import Data.Maybe                    (isJust, isNothing, listToMaybe,
                                      maybeToList)
import Empire.ASTOp                  (runASTOp)
import Empire.Commands.GraphBuilder  (buildClassGraph, buildConnections,
                                      buildGraph, buildNodes, getNodeCode,
                                      getNodeName)
import Empire.Data.AST               (SomeASTException,
                                      astExceptionFromException,
                                      astExceptionToException)
import Luna.Package                  (findPackageFileForFile,
                                      findPackageRootForFile,
                                      getRelativePathForModule, includedLibs)
import LunaStudio.API.Response       (InverseOf, ResultOf)
import LunaStudio.Data.Breadcrumb    (Breadcrumb (..))
import LunaStudio.Data.Connection    (Connection (..))
import LunaStudio.Data.Diff          (Diff, diff)
import LunaStudio.Data.GraphLocation (GraphLocation (..))
import LunaStudio.Data.NodeLoc       (NodeLoc (..))
import LunaStudio.Data.PortRef       (InPortRef (..), OutPortRef (..), AnyPortRef (..))
import LunaStudio.Data.Node          (ExpressionNode (..), NodeId)
import LunaStudio.Data.Port          (InPort (..), InPortIndex (..),
                                      OutPort (..), OutPortIndex (..),
                                      Port (..), PortState (..), getPortNumber)
import LunaStudio.Data.Project       (LocationSettings)

import Empire.Empire         (Empire)
import LunaStudio.Data.Graph (Graph (Graph))

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

instance Modification AddNode.Request where
    perform (AddNode.Request loc nl expression nodeMeta connectTo) =
        withDiff loc
            $ Graph.addNodeWithConnection loc nl expression nodeMeta connectTo
    buildInverse (AddNode.Request loc nl _ _ _)
        = pure $ RemoveNodes.Request loc [nl]

instance Modification RemoveNodes.Request where
    perform (RemoveNodes.Request location nodeLocs) =
        withDiff location $ Graph.removeNodes location $ convert <$> nodeLocs
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
        pure $ AddSubgraph.Request location nodes conns

getSrcPortByNodeId :: NodeId -> OutPortRef
getSrcPortByNodeId nid = OutPortRef (NodeLoc def nid) []

getDstPortByNodeLoc :: NodeLoc -> AnyPortRef
getDstPortByNodeLoc nl = InPortRef' $ InPortRef nl [Self]


instance Modification AddConnection.Request where
    perform (AddConnection.Request location src' dst') = withDiff location $ do
        let getSrcPort = either id getSrcPortByNodeId
            getDstPort = either id getDstPortByNodeLoc
        void $ Graph.connectCondTC True location (getSrcPort src')
                                                 (getDstPort dst')
    buildInverse (AddConnection.Request location _ dst') = do
        let dstNodeId = either (view PortRef.nodeId) (view NodeLoc.nodeId) dst'
        prevExpr <- Graph.withGraph location . runASTOp $ getNodeCode dstNodeId
        pure $ SetNodeExpression.Request location dstNodeId prevExpr

instance Modification RenamePort.Request where
    perform (RenamePort.Request location portRef name)
        = withDiff location $ Graph.renamePort location portRef name
    buildInverse (RenamePort.Request location portRef name) = do
        oldName <- Graph.getPortName location portRef
        pure $ RenamePort.Request location portRef oldName

instance Modification RenameNode.Request where
    perform (RenameNode.Request location nodeId name)
        = withDiff location $ Graph.renameNode location nodeId name
    buildInverse (RenameNode.Request location nodeId name) = do
        prevName <- Graph.getName location nodeId
        pure $ RenameNode.Request location nodeId $ fromMaybe "" prevName

instance Modification SetPortDefault.Request where
    perform (SetPortDefault.Request location portRef defaultValue)
        = withDiff location $ Graph.setPortDefault location portRef defaultValue
    buildInverse (SetPortDefault.Request location portRef _)
        = SetPortDefault.Request location portRef
            <$> Graph.getPortDefault location portRef

instance Modification AddImports.Request where
    perform (AddImports.Request location modules)
        = withDiff location $ Graph.addImports location modules

instance Modification AddPort.Request where
    perform (AddPort.Request location portRef connsDst name)
        = withDiff location
            $ Graph.addPortWithConnections location portRef name connsDst
    buildInverse (AddPort.Request location portRef _ _)
        = pure $ RemovePort.Request location portRef

instance Modification AddSubgraph.Request where
    perform (AddSubgraph.Request location nodes connections)
        = withDiff location $ Graph.addSubgraph location nodes connections
    buildInverse (AddSubgraph.Request location nodes connections)
        = pure $ RemoveNodes.Request
            location
            ((convert . view Node.nodeId) <$> nodes)

instance Modification AutolayoutNodes.Request where
    perform (AutolayoutNodes.Request location nodeLocs _)
        = withDiff location
            $ Graph.autolayoutNodes location (convert <$> nodeLocs)
    buildInverse (AutolayoutNodes.Request location nodeLocs _) = do
        positions <- Graph.getNodeMetas location nodeLocs
        pure $ SetNodesMeta.Request location $ Map.fromList $
            map (over _1 convert) $ catMaybes positions

instance Modification CollapseToFunction.Request where
    perform (CollapseToFunction.Request location locs) = withDiff location $ do
        let ids = convert <$> locs
        Graph.collapseToFunction location ids
    buildInverse (CollapseToFunction.Request (GraphLocation file _) _) = do
        code <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        cache <- Graph.prepareNodeCache (GraphLocation file def)
        pure $ SetCode.Request (GraphLocation file def) code cache

instance Modification Copy.Request where
    perform (Copy.Request location nodeLocs) = do
        r <- Graph.prepareCopy location (convert nodeLocs)
        pure $ Copy.Result r r --FIXME

instance Modification DumpGraphViz.Request where
    perform (DumpGraphViz.Request location)
        = Graph.dumpGraphViz location

instance Modification GetSubgraphs.Request where
    perform (GetSubgraphs.Request location) = do
        graph <- Graph.getGraph location
        let bc = location ^.
                GraphLocation.breadcrumb . Breadcrumb.items . to unsafeLast
        pure . GetSubgraphs.Result $ Map.singleton bc graph

instance Modification MovePort.Request where
    perform (MovePort.Request location portRef newPortPos)
        = withDiff location $ Graph.movePort location portRef newPortPos
    buildInverse (MovePort.Request location (OutPortRef nl pid) newPortPos)
        = let prevPos = last pid
              toBePos = init pid <> [prevPos]
          in pure $
                MovePort.Request location
                    (OutPortRef nl toBePos)
                    (coerce newPortPos)

instance Modification Paste.Request where
    perform (Paste.Request location position string)
        = withDiff location $ Graph.paste location position string
    buildInverse (Paste.Request location position string)
        = pure $ RemoveNodes.Request location (error "empty remove nodes")

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

instance Modification RemoveConnection.Request where
    perform (RemoveConnection.Request location dst) = withDiff location $ do
        mayDstNode <- getNodeById location $ dst ^. PortRef.dstNodeId
        () <- when (isNothing mayDstNode)
            $ throwM $ DestinationDoesNotExistException dst
        Graph.disconnect location dst
    buildInverse (RemoveConnection.Request location dst) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        case find (\conn -> snd conn == dst) connections of
            Nothing         -> throwM $ ConnectionDoesNotExistException dst
            Just (src, dst) -> pure $
                AddConnection.Request location (Left src) (Left $ InPortRef' dst)

data SidebarDoesNotExistException = SidebarDoesNotExistException
    deriving (Show)

instance Exception SidebarDoesNotExistException where
    fromException = astExceptionFromException
    toException = astExceptionToException

instance Modification RemovePort.Request where
    perform (RemovePort.Request location portRef) = withDiff location $ do
        maySidebar <- view GraphAPI.inputSidebar <$> Graph.getGraphNoTC location
        () <- when (isNothing maySidebar) $ throwM SidebarDoesNotExistException
        Graph.removePort location portRef
    buildInverse (RemovePort.Request location portRef) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        oldName     <- Graph.getPortName location portRef
        let conns = flip filter connections $ (== portRef) . fst
        pure $ AddPort.Request
            location
            portRef
            (map (InPortRef' . snd) conns)
            (Just oldName)

instance Modification SetCode.Request where
    perform (SetCode.Request location@(GraphLocation file _) code cache)
        = withDiff location $ do
            Graph.withUnit (GraphLocation file def) $ Graph.nodeCache .= cache
            Graph.loadCode location code
            Graph.resendCode location
            Graph.typecheck location
    buildInverse (SetCode.Request location@(GraphLocation file _) _ _) = do
        cache <- Graph.prepareNodeCache location
        code  <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        pure $ SetCode.Request location code cache

instance Modification SaveSettings.Request where
    perform (SaveSettings.Request gl settings) = saveSettings gl settings gl

instance Modification SetNodeExpression.Request where
    perform (SetNodeExpression.Request location nodeId expression)
        = withDiff location $ do
            Graph.setNodeExpression location nodeId expression
            Graph.typecheck location
    buildInverse (SetNodeExpression.Request location nodeId _) = do
        oldExpr <- Graph.withGraph location . runASTOp $
            getNodeCode nodeId
        pure $ SetNodeExpression.Request location nodeId oldExpr

instance Modification SetNodesMeta.Request where
    perform (SetNodesMeta.Request location updates) = withDiff location $ do
        for_ (toList updates) $ uncurry $ Graph.setNodeMeta location
    buildInverse (SetNodesMeta.Request location updates) = do
        allNodes <- Graph.withBreadcrumb location (runASTOp buildNodes)
            $ view GraphAPI.nodes <$> runASTOp buildClassGraph
        let prevMeta = Map.fromList . catMaybes . flip fmap allNodes $ \node ->
                justIf
                    (Map.member (node ^. Node.nodeId) updates)
                    (node ^. Node.nodeId, node ^. Node.nodeMeta)
        pure $ SetNodesMeta.Request location prevMeta

instance Modification Substitute.Request where
    perform (Substitute.Request location diffs) = do
        let file = location ^. GraphLocation.filePath
        withDiff location $ Graph.substituteCodeFromPoints file diffs

instance Modification GetBuffer.Request where
    perform (GetBuffer.Request file) = do
        code <- Graph.getBuffer file
        pure $ GetBuffer.Result code

instance Modification Interpreter.Request where
    perform (Interpreter.Request gl command)
        = interpreterAction command gl where
        interpreterAction Interpreter.Start  = Graph.startInterpreter
        interpreterAction Interpreter.Pause  = Graph.pauseInterpreter
        interpreterAction Interpreter.Reload = Graph.reloadInterpreter

-- === Utils -> ToRefactor === --

catchAllExceptions :: Empire a -> Empire (Either SomeException a)
catchAllExceptions act = try act

withDiff ::  GraphLocation -> Empire a -> Empire Diff
withDiff location action = do
    oldGraph <- (_Left %~ Graph.prepareGraphError)
        <$> catchAllExceptions (Graph.getGraphNoTC location)
    void action
    newGraph <- (_Left %~ Graph.prepareGraphError)
        <$> catchAllExceptions (Graph.getGraphNoTC location)
    pure $ diff oldGraph newGraph

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

logProjectPathNotFound :: MonadIO m => m ()
logProjectPathNotFound
    = Project.logProjectSettingsError "Could not find project path."

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

getAllNodes :: GraphLocation -> Empire [Node.Node]
getAllNodes location = do
    graph <- Graph.getGraph location
    let inputSidebarList  = maybeToList $ graph ^. GraphAPI.inputSidebar
        outputSidebarList = maybeToList $ graph ^. GraphAPI.outputSidebar
    pure $ fmap Node.ExpressionNode' (graph ^. GraphAPI.nodes)
        <> fmap Node.InputSidebar'   inputSidebarList
        <> fmap Node.OutputSidebar'  outputSidebarList

getNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.Node]
getNodesByIds location nids = filterRelevantNodes <$> getAllNodes location where
    filterRelevantNodes
        = filter (flip Set.member requestedIDs . view Node.nodeId)
    requestedIDs = fromList nids

getExpressionNodesByIds :: GraphLocation -> [NodeId] -> Empire [ExpressionNode]
getExpressionNodesByIds location nids
    = filterRelevantNodes <$> Graph.getNodes location where
        filterRelevantNodes
            = filter (flip Set.member requestedIDs . view Node.nodeId)
        requestedIDs = fromList nids

getNodeById :: GraphLocation -> NodeId -> Empire (Maybe Node.Node)
getNodeById location nid
    = find (\n -> n ^. Node.nodeId == nid) <$> getAllNodes location

getProjectPathAndRelativeModulePath :: MonadIO m
    => FilePath -> m (Maybe (FilePath, FilePath))
getProjectPathAndRelativeModulePath modulePath = do
    let eitherToMaybe :: MonadIO m
            => Either Path.PathException (Path.Path Path.Abs Path.File)
            -> m (Maybe (Path.Path Path.Abs Path.File))
        eitherToMaybe (Left  e) = Project.logProjectSettingsError e >> pure def
        eitherToMaybe (Right a) = pure $ Just a
    mayProjectPathAndRelModulePath <- liftIO . runMaybeT $ do
        absModulePath  <- MaybeT $
            eitherToMaybe =<< try (Path.parseAbsFile modulePath)
        absProjectPath <- MaybeT $ findPackageFileForFile absModulePath
        relModulePath  <- MaybeT $
            getRelativePathForModule absProjectPath absModulePath
        pure (Path.fromAbsFile absProjectPath, Path.fromRelFile relModulePath)
    when (isNothing mayProjectPathAndRelModulePath) logProjectPathNotFound
    pure mayProjectPathAndRelModulePath

saveSettings :: GraphLocation -> LocationSettings -> GraphLocation -> Empire ()
saveSettings gl settings newGl = handle logError action where
    logError :: MonadIO m => SomeException -> m ()
    logError e = Project.logProjectSettingsError e
    action     = do
        bc    <- Breadcrumb.toNames <$> Graph.decodeLocation gl
        newBc <- Breadcrumb.toNames <$> Graph.decodeLocation newGl
        let filePath        = gl    ^. GraphLocation.filePath
            newFilePath     = newGl ^. GraphLocation.filePath
            lastBcInOldFile = if filePath == newFilePath then newBc else bc
        withJustM (getProjectPathAndRelativeModulePath filePath) $ \(cp, fp) ->
            Project.updateLocationSettings cp fp bc settings lastBcInOldFile
        when (filePath /= newFilePath)
            $ withJustM (getProjectPathAndRelativeModulePath newFilePath)
                $ \(cp, fp) ->
                    Project.updateCurrentBreadcrumbSettings cp fp newBc

getClosestBcLocation :: GraphLocation -> Breadcrumb Text -> Empire GraphLocation
getClosestBcLocation gl (Breadcrumb []) = pure gl
getClosestBcLocation gl (Breadcrumb (nodeName:newBcItems)) = do
    g <- Graph.getGraph gl
    let mayN = find ((Just nodeName ==) . view Node.name) (g ^. GraphAPI.nodes)
        processLocation n = do
            let nid = n ^. Node.nodeId
                bci = if n ^. Node.isDefinition
                    then Breadcrumb.Definition nid
                    else Breadcrumb.Lambda nid
                nextLocation = gl & GraphLocation.breadcrumb . Breadcrumb.items
                    %~ (<>[bci])
            getClosestBcLocation nextLocation $ Breadcrumb newBcItems
    maybe (pure gl) processLocation mayN

