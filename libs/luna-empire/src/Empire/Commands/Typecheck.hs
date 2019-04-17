module Empire.Commands.Typecheck where

import Empire.Prelude hiding (mod)

import qualified Control.Concurrent.Async           as Async
import qualified Data.Bimap                         as Bimap
import qualified Data.Graph.Data.Component.Vector   as ComponentVector
import qualified Data.Graph.Data.Layer.Layout       as Layout
import qualified Data.Graph.Store                   as Store
import qualified Data.Graph.Store.Buffer            as Buffer
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.Commands.GraphBuilder       as GraphBuilder
import qualified Empire.Commands.Publisher          as Publisher
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Empire                      as Empire
import qualified Empire.Utils.ValueListener         as Listener
import qualified Luna.Datafile.Stdlib               as StdLocator
import qualified Luna.IR                            as IR
import qualified Luna.IR.Aliases                    as Uni
import qualified Luna.Package                       as Package
import qualified Luna.Package.Environment           as PackageEnv
import qualified Luna.Package.Structure.Name        as Package
import qualified Luna.Pass.Data.Error               as Error
import qualified Luna.Pass.Evaluation.Data.Scope    as Scope
import qualified Luna.Pass.Evaluation.Interpreter   as Interpreter
import qualified Luna.Pass.Flow.ProcessUnits        as ProcessUnits
import qualified Luna.Pass.Preprocess.PreprocessDef as Prep
import qualified Luna.Pass.Resolve.Data.Resolution  as Resolution
import qualified Luna.Pass.Scheduler                as Scheduler
import qualified Luna.Pass.Sourcing.Data.Class      as Class
import qualified Luna.Pass.Sourcing.Data.Def        as Def
import qualified Luna.Pass.Sourcing.Data.Unit       as Unit
import qualified Luna.Pass.Sourcing.ImportsPlucker  as ImportsPlucker
import qualified Luna.Pass.Sourcing.UnitLoader      as UnitLoader
import qualified Luna.Pass.Sourcing.UnitMapper      as UnitMapper
import qualified Luna.Pass.Typing.Data.Target       as Target
import qualified Luna.Pass.Typing.Typechecker       as TC
import qualified Luna.Runtime                       as Runtime
import qualified Luna.Std                           as Std
import qualified LunaStudio.Data.Error              as APIError
import qualified LunaStudio.Data.GraphLocation      as GraphLocation
import qualified Memory                             as Memory
import qualified Path

import Control.Concurrent.Async         (Async)
import Control.Exception.Safe           (mask_, tryAny)
import Control.Lens                     (uses)
import Control.Monad.Reader             (ask, runReaderT)
import Data.Graph.Component.Node.Class  (Nodes)
import Data.Graph.Data.Component.Class  (Component (Component))
import Data.Map                         (Map)
import Data.Maybe                       (catMaybes)
import Empire.ASTOp                     (liftScheduler, runASTOp)
import Empire.Commands.Graph.Breadcrumb (runInternalBreadcrumb,
                                         withRootedFunction)
import Empire.Data.AST                  (NodeRef)
import Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import Empire.Data.Graph                (Graph)
import Empire.Empire                    (Command, InterpreterEnv, zoomCommand)
import Empire.Utils.ValueListener       (SingleRep (ErrorRep, SuccessRep),
                                         ValueRep (OneTime, Streaming))
import Foreign.Ptr                      (plusPtr)
import Luna.Pass.Data.Stage             (Stage)
import Luna.Pass.Evaluation.Data.Scope  (LocalScope)
import LunaStudio.Data.Breadcrumb       (Breadcrumb (Breadcrumb),
                                         BreadcrumbItem (Definition))
import LunaStudio.Data.GraphLocation    (GraphLocation (GraphLocation))
import LunaStudio.Data.NodeValue        (NodeValue (NodeError, NodeValue))
import LunaStudio.Data.Visualization    (VisualizationValue (StreamDataPoint, StreamStart, Value))
import System.Directory                 (canonicalizePath, withCurrentDirectory)
import System.Environment               (getEnv)
import System.FilePath                  (takeDirectory)



runTC :: IR.Qualified -> GraphLocation -> Command InterpreterEnv ()
runTC modName (GraphLocation _ br) = do
    typed <- use $ Graph.userState . Empire.typedUnits
    unit  <- use $ Graph.userState . Empire.clsGraph . Graph.clsClass
    imps  <- liftScheduler $ ImportsPlucker.run unit
    ress  <- use $ Graph.userState . Empire.resolvers
    let relevantResolvers = Map.restrictKeys ress
            (Set.fromList $ essentialImports <> [modName] <> imps)
        resolver          = mconcat . Map.elems $ relevantResolvers
    zoomCommand Empire.clsGraph $ case br of
        Breadcrumb (Definition uuid:_) -> do
            root <- preuse $ Graph.userState . Graph.clsFuns . ix uuid . Graph.funGraph . Graph.breadcrumbHierarchy . BH.self
            case root of
                Just r -> liftScheduler $ do
                    Prep.preprocessDef resolver r
                    TC.runTypechecker def r typed
                _      -> error $ "runTC: wrong breadcrumb " <> show br
            pure ()
        Breadcrumb _                   -> pure ()
 -- do
    -- let currentTarget = TgtDef (convert moduleName) (convert functionName)
    -- runTypecheck currentTarget imports
    -- runASTOp $ do
    --     mapping <- unwrap <$> IR.getAttr @ExprMapping
    --     Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    -- return ()
    --
withPackageCurrentDirectory :: FilePath -> IO a -> IO a
withPackageCurrentDirectory currentFile act = do
    rootPath <- liftIO $ Package.findPackageRootForFile =<< Path.parseAbsFile currentFile
    let packageDirectory = maybe (takeDirectory currentFile) Path.toFilePath rootPath
    withCurrentDirectory packageDirectory act

runInterpreter :: FilePath -> Runtime.Units -> Command Graph (Maybe LocalScope)
runInterpreter path imports = do
    selfRef    <- use    $ Graph.userState . Graph.breadcrumbHierarchy . BH.self
    bodyRefMay <- runASTOp $ matchExpr selfRef $ \case
        Uni.Function _ as b -> do
            args <- ComponentVector.toList as
            if null args then Just <$> IR.source b else pure Nothing
        _ -> return Nothing
    interpreted <- for bodyRefMay $ \bodyRef -> liftScheduler $ Interpreter.execInterpreter bodyRef imports
    liftIO $ for interpreted $ \res ->
        mask_ $ withPackageCurrentDirectory path res

resolveSymbol :: Map IR.Qualified Unit.Unit -> Target.Target
    -> Maybe (IR.Term IR.Function)
resolveSymbol units tgt = do
    symbolRef <- case tgt of
        Target.Function mod function   -> do
            moduleDefs <- units ^? ix mod . Unit.definitions
            defRef     <- moduleDefs ^? wrapped . ix function
            pure defRef
        Target.Method mod klass method -> do
            moduleClasses <- units ^? ix mod . Unit.classes
            classRef      <- moduleClasses ^? ix klass . Def.documented
            methodRef     <- classRef ^? Class.methods . wrapped . ix method
            pure methodRef
        Target.Unknown                 -> Nothing
    symbolBody <- symbolRef ^? Def.documented . Def._Sourced . Def.body
    pure symbolBody

makeError :: Error.CompileError -> NodeValue
makeError err =
    let toSrcLoc (Target.Method   mod klass method) =
            APIError.SourceLocation
                (convertVia @String mod)
                (Just (convert klass))
                (convert method)
        toSrcLoc (Target.Function mod function)     =
            APIError.SourceLocation
                (convertVia @String mod)
                Nothing
                (convert function)
        toSrcLoc Target.Unknown                     =
            APIError.SourceLocation
                "unknown module"
                Nothing
                "unknown function"
        errorDetails =
            APIError.CompileErrorDetails
                (map toSrcLoc (err ^. Error.failedAt))
                (map toSrcLoc (err ^. Error.arisingFrom))
        nodeError    = NodeError $
            APIError.Error
                (APIError.CompileError errorDetails)
                (err ^. Error.contents)
    in nodeError

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc@(GraphLocation _ br) = case br of
    Breadcrumb (Definition uuid:rest) -> do
        units <- use $ Graph.userState . Empire.mappedUnits
        zoomCommand Empire.clsGraph $ withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb rest) $ do
            (inEdge, outEdge) <- use $ Graph.userState . Graph.breadcrumbHierarchy . BH.portMapping
            (updates, errors) <- runASTOp $ do
                inputUpdate  <-
                    GraphBuilder.buildInputSidebarTypecheckUpdate  inEdge
                outputUpdate <-
                    GraphBuilder.buildOutputSidebarTypecheckUpdate outEdge
                allNodeIds   <- uses Graph.breadcrumbHierarchy topLevelIDs
                let resolveFun = resolveSymbol units
                (updates, errors) <- unzip <$> forM allNodeIds (\nid -> do
                    err      <- Error.getError =<< ASTRead.getASTRef nid
                    tcUpdate <- GraphBuilder.buildNodeTypecheckUpdate
                        resolveFun nid
                    let nodeError = ((nid,) . makeError) <$> err
                    pure (tcUpdate, nodeError))
                pure (inputUpdate : outputUpdate : updates, errors)
            mask_ $ do
                traverse_ (Publisher.notifyNodeTypecheck loc) updates
                for_ (catMaybes errors) $ \(nid, e) ->
                    Publisher.notifyResultUpdate loc nid e 0
    Breadcrumb _ -> pure ()

updateValues :: GraphLocation
             -> LocalScope
             -> Command Graph [Async ()]
updateValues loc@(GraphLocation path _) scope = do
    childrenMap <- use $ Graph.userState . Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    nodes <- runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, _tgt) -> do
        pointer <- ASTRead.getASTPointer nid
        matchExpr pointer $ \case
            Uni.Unify{} -> Just . (nid,) <$> ASTRead.getVarNode pointer
            _           -> return $ Just (nid, pointer)
    let send nid m = flip runReaderT env $
            Publisher.notifyResultUpdate loc nid m 0
        sendRep nid (ErrorRep e)     = send nid
                                     $ NodeError
                                     $ APIError.Error APIError.RuntimeError e
        sendRep nid (SuccessRep s l) = send nid
                                     $ NodeValue s
                                     $ Value <$> l
        sendStreamRep nid a@(ErrorRep _)   = sendRep nid a
        sendStreamRep nid (SuccessRep s l) = send nid
                                           $ NodeValue s
                                           $ StreamDataPoint <$> l
    asyncs <- liftIO $ withPackageCurrentDirectory path $
        forM nodes $ \(nid, ref) -> do
            let resVal = Scope.localLookup ref scope
            liftIO $ forM resVal $ \v -> do
                value <- Listener.getReps v
                case value of
                    OneTime r   -> Async.async $ sendRep nid r
                    Streaming f -> do
                        send nid (NodeValue "Stream" $ Just StreamStart)
                        Async.async (f (sendStreamRep nid))
    return $ catMaybes asyncs

essentialImports :: [IR.Qualified]
essentialImports = ["Std.Primitive", "Std.Base"]

filePathToQualName :: MonadIO m => FilePath -> m IR.Qualified
filePathToQualName path = liftIO $ do
    path' <- Path.parseAbsFile path
    root  <- Package.packageRootForFile path'
    let projName = Package.getPackageName root
    file  <- Path.stripProperPrefix (root Path.</> $(Path.mkRelDir "src")) path'
    return $ Package.mkQualName projName file

prepareFileImportPaths :: MonadIO m => FilePath -> m (Map IR.Qualified FilePath)
prepareFileImportPaths file = liftIO $ do
    filePath        <- Path.parseAbsFile file
    currentProjPath <- Package.packageRootForFile filePath
    stdPath         <- StdLocator.findPath
    libs            <- Package.packageImportPaths currentProjPath stdPath
    PackageEnv.setLibraryVars libs
    srcs            <- for (snd <$> libs) $ \libPath -> do
        p <- Path.parseAbsDir libPath
        fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources p
    pure $ Map.unions srcs

stop :: Command InterpreterEnv ()
stop = do
    cln     <- use $ Graph.userState . Empire.cleanUp
    threads <- use $ Graph.userState . Empire.listeners
    Graph.userState . Empire.listeners .= []
    liftIO $ mapM_ Async.uninterruptibleCancel threads
    liftIO cln

translate :: Buffer.RedirectMap -> Int -> NodeRef -> NodeRef
translate redMap off node = wrap $ (unwrap (redMap Map.! (Memory.Ptr $ convert node)))
    `plusPtr` off

primStdModuleName :: IR.Qualified
primStdModuleName = "Std.Primitive"

makePrimStdIfMissing :: Command InterpreterEnv ()
makePrimStdIfMissing = do
    existingStd <- use $ Graph.userState . Empire.resolvers . at primStdModuleName
    when_ (isNothing existingStd) $ tryAny $ do
        (mods, finalizer, typed, computed, ress, units) <- liftScheduler $ do
            (fin, stdUnitRef) <- Std.stdlib @Stage
            stdPath <- (Path.</> $(Path.mkRelDir "Std")) <$> StdLocator.findPath
            srcs    <- fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources stdPath
            UnitLoader.init
            Scheduler.registerAttr @Unit.UnitRefsMap
            Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton "Std.Primitive" stdUnitRef
            for Std.stdlibImports $ UnitLoader.loadUnit def srcs []
            Unit.UnitRefsMap mods <- Scheduler.getAttr
            units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
                Unit.Graph r       -> UnitMapper.mapUnit n r
                Unit.Precompiled p -> pure p
            let unitResolvers   = Map.mapWithKey Resolution.resolverFromUnit units
                importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped (essentialImports <>)
                                                                                            . view Unit.imports <$> mods
                unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
            (typed, evald) <- ProcessUnits.processUnits def def unitsWithResolvers
            return (mods, fin, typed, evald, unitResolvers, units)
        zoomCommand Empire.clsGraph $ runASTOp $ for mods $ \u -> case u ^. Unit.root of
            Unit.Graph r -> IR.deleteSubtree r
            _            -> return ()
        Graph.userState . Empire.cleanUp      .= finalizer
        Graph.userState . Empire.typedUnits   .= typed
        Graph.userState . Empire.mappedUnits  .= units
        Graph.userState . Empire.runtimeUnits .= computed
        Graph.userState . Empire.resolvers    .= ress

ensureCurrentScope :: Bool -> IR.Qualified -> FilePath -> NodeRef -> Command InterpreterEnv ()
ensureCurrentScope recompute modName path root = do
    existing <- use $ Graph.userState . Empire.resolvers . at modName
    when (recompute || isNothing existing) $ do
        compileCurrentScope modName path root

compileCurrentScope :: IR.Qualified -> FilePath -> NodeRef -> Command InterpreterEnv ()
compileCurrentScope modName path root = do
    typed   <- use $ Graph.userState . Empire.typedUnits
    evald   <- use $ Graph.userState . Empire.runtimeUnits
    ress    <- use $ Graph.userState . Empire.resolvers

    (mods, newTyped, newEvald, newResolvers, units) <- liftScheduler $ do
        imports <- ImportsPlucker.run root
        UnitLoader.init
        srcs <- prepareFileImportPaths path
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton modName $ Unit.UnitRef (Unit.Graph $ Layout.unsafeRelayout root) (wrap imports)
        for imports $ UnitLoader.loadUnitIfMissing (Set.fromList $ Map.keys ress) srcs [modName]

        Unit.UnitRefsMap mods <- Scheduler.getAttr
        units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
            Unit.Graph r       -> UnitMapper.mapUnit n r
            Unit.Precompiled p -> pure p

        let unitResolvers   = Map.union (Map.mapWithKey Resolution.resolverFromUnit units) ress
            importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped (essentialImports <>)
                                                                                        . view Unit.imports <$> mods
            unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
        (newTyped, newEvald) <- ProcessUnits.processUnits typed evald unitsWithResolvers
        return (mods, newTyped, newEvald, unitResolvers, units)
    for mods $ \u -> zoomCommand Empire.clsGraph $ runASTOp $ case u ^. Unit.root of
        Unit.Graph r -> when (Layout.relayout r /= root) $ IR.deleteSubtree r
        _            -> return ()

    Graph.userState . Empire.mappedUnits  %= Map.union units
    Graph.userState . Empire.typedUnits   .= newTyped
    Graph.userState . Empire.runtimeUnits .= newEvald
    Graph.userState . Empire.resolvers    .= newResolvers

run :: GraphLocation
    -> Graph.ClsGraph
    -> Store.RootedWithRedirects NodeRef
    -> Bool
    -> Bool
    -> Command InterpreterEnv ()
run gl clsGraph rooted interpret recompute = do
    root <- runNoCleanUp gl clsGraph rooted interpret recompute
    zoomCommand Empire.clsGraph . runASTOp $ IR.deleteSubtree root

runNoCleanUp
    :: GraphLocation
    -> Graph.ClsGraph
    -> Store.RootedWithRedirects NodeRef
    -> Bool
    -> Bool
    -> Command InterpreterEnv (Component Nodes ())
runNoCleanUp gl clsGraph rooted interpret recompute = do
    stop
    (root, redirects)
        <- runASTOp . Store.deserializeWithRedirects $ rooted ^. Store.rooted
    let filePath        = gl ^. GraphLocation.filePath
        deserializerOff = redirects Map.! someTypeRep @IR.Terms
        newClsGraph     = clsGraph & BH.refs %~ translate
            (rooted ^. Store.redirections)
            deserializerOff
    Graph.userState . Empire.clsGraph .= newClsGraph
    modName <- filePathToQualName filePath
    makePrimStdIfMissing
    ensureCurrentScope recompute modName filePath root
    runTC modName gl
    updateNodes gl
    let processBC evald (Breadcrumb (Definition uuid:r))
            = zoomCommand Empire.clsGraph
            . withRootedFunction uuid
            . runInternalBreadcrumb (Breadcrumb r) $ do
                scope <- runInterpreter filePath evald
                traverse (updateValues gl) scope
        processBC _ _ = pure Nothing
    when interpret $ do
        evald  <- use $ Graph.userState . Empire.runtimeUnits
        asyncs <- processBC evald $ gl ^. GraphLocation.breadcrumb
        Graph.userState . Empire.listeners .= fromMaybe mempty asyncs
    pure root
