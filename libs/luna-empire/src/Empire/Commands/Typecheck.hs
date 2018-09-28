{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.Typecheck where

import Control.Lens (uses, element)
import           Control.Arrow                    ((***), (&&&))
import           Control.Concurrent.Async         (Async)
import qualified Control.Concurrent.Async         as Async
import           Control.Concurrent               (MVar, readMVar)
import qualified Control.Concurrent.MVar.Lifted   as Lifted
import           Control.Exception.Safe           (mask_)
import           Control.Monad                    (void)
import           Control.Monad.Except             hiding (when)
import           Control.Monad.Reader             (ask, runReaderT)
import           Control.Monad.State              (execStateT)
import           Data.Graph.Data.Component.Class  (Component(..))
import qualified Data.Graph.Store                 as Store
import qualified Data.Graph.Store.Buffer          as Buffer
import qualified Data.IORef                       as IORef
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, maybeToList)
import           Empire.Prelude                   hiding (mapping, toList)
import           Foreign.Ptr                      (plusPtr)
import           System.Directory                 (canonicalizePath, withCurrentDirectory)
import           System.Environment               (getEnv)
import           System.FilePath                  (takeDirectory)
import qualified Path

import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR                          as IR
import qualified Luna.Pass.Typing.Typechecker     as TC
import qualified Luna.Pass.Preprocess.PreprocessDef as Prep
import qualified Memory                           as Memory
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem(Definition))
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import qualified LunaStudio.Data.Error            as APIError
import           LunaStudio.Data.NodeValue        (NodeValue (..))
import           LunaStudio.Data.Visualization    (VisualizationValue (..))

import           Empire.ASTOp                     (liftScheduler, runASTOp)
-- import           Empire.ASTOp                     (getImportedModules, runASTOp, runTypecheck, runModuleTypecheck)
import qualified Empire.ASTOps.Read               as ASTRead
import           Empire.Commands.Breadcrumb       (runInternalBreadcrumb, withRootedFunction)
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.AST                  (NodeRef)
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Empire

import qualified System.IO as IO
import qualified Luna.Debug.IR.Visualizer as Vis

import qualified Data.Graph.Data.Component.Vector  as ComponentVector
import qualified Luna.Pass.Resolve.Data.Resolution as Resolution
import qualified Luna.Pass.Sourcing.UnitMapper     as UnitMapper
import           Luna.Pass.Data.Stage (Stage)
import qualified Data.Bimap as Bimap
import qualified Luna.Pass.Flow.ProcessUnits       as ProcessUnits
import qualified Data.Graph.Data.Layer.Layout      as Layout

import qualified Luna.Pass.Data.Error              as Error
import qualified Luna.Pass.Typing.Data.Target      as Target
import qualified Luna.Pass.Scheduler               as Scheduler
import qualified Luna.Pass.Sourcing.UnitLoader     as UnitLoader
import qualified Luna.Pass.Sourcing.ImportsPlucker as ImportsPlucker
import qualified Luna.Pass.Evaluation.Data.Scope   as Scope
import qualified Luna.Pass.Evaluation.Interpreter  as Interpreter
import qualified Luna.Std                          as Std
import qualified Luna.Pass.Sourcing.Data.Def       as Def
import qualified Luna.Pass.Sourcing.Data.Unit      as Unit
import qualified Luna.Runtime                      as Runtime

import qualified Data.Set as Set

import Empire.Utils.ValueListener (ValueRep (..), SingleRep (..))
import qualified Empire.Utils.ValueListener as Listener

import Luna.Pass.Evaluation.Data.Scope (LocalScope)
import Data.Map (Map)
-- import           Luna.Builtin.Data.LunaEff        (runError, runIO)
-- import           Luna.Builtin.Data.Module         (Imports (..), unionImports, unionsImports)
-- import           Luna.Builtin.Prim                (SingleRep (..), ValueRep (..), getReps)
-- import qualified Luna.Compilation                 as Compilation
import qualified Luna.Package                     as Package
import qualified Luna.Package.Structure.Name      as Package
-- import           Luna.Compilation                 (CompiledModules (..))
import qualified Luna.IR                          as IR
-- import           Luna.Pass.Data.ExprMapping
-- import qualified Luna.Pass.Evaluation.Interpreter as Interpreter
-- import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget(TgtDef))
-- import qualified Luna.IR.Layer.Errors             as Errors
-- import           OCI.IR.Name.Qualified            (QualName)

runTC :: IR.Qualified -> GraphLocation -> Command InterpreterEnv ()
runTC modName (GraphLocation file br) = do
    typed <- use $ Graph.userState . typedUnits
    root  <- use $ Graph.userState . clsGraph . Graph.clsClass
    imps  <- liftScheduler $ ImportsPlucker.run root
    ress  <- use $ Graph.userState . resolvers
    let relevantResolvers = Map.restrictKeys ress (Set.fromList $ essentialImports <> [modName] <> imps)
        resolver = mconcat . Map.elems $ relevantResolvers
    zoomCommand clsGraph $ case br of
        Breadcrumb (Definition uuid:_) -> do
            cls <- use $ Graph.userState
            let Just root = cls ^? Graph.clsFuns . ix uuid . Graph.funGraph . Graph.breadcrumbHierarchy . BH.self
            liftScheduler $ do
                Prep.preprocessDef resolver root
                TC.runTypechecker def root typed
            return ()
        Breadcrumb _                   -> return ()
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
    rootPath   <- liftIO $ Package.findPackageRootForFile =<< Path.parseAbsFile path
    selfRef    <- use    $ Graph.userState . Graph.breadcrumbHierarchy . BH.self
    bodyRefMay <- runASTOp $ matchExpr selfRef $ \case
        Uni.Function _ as b -> do
            args <- ComponentVector.toList as
            if null args then Just <$> IR.source b else pure Nothing
        _ -> return Nothing
    interpreted <- for bodyRefMay $ \bodyRef -> liftScheduler $ Interpreter.execInterpreter bodyRef imports
    liftIO $ for interpreted $ \res ->
        mask_ $ withPackageCurrentDirectory path res

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc@(GraphLocation _ br) = case br of
    Breadcrumb (Definition uuid:rest) -> do
        units <- use $ Graph.userState . mappedUnits
        let resolveFun mod n =
                let moduleDefs = units ^? ix mod . to Unit._definitions
                    defRef     = moduleDefs >>= \a -> a ^? wrapped . ix n
                    docTerm    = view Def.documented <$> defRef
                    defBody    = docTerm >>= \a -> a ^? Def._Body
                in defBody
        zoomCommand clsGraph $ withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb rest) $ do
            (inEdge, outEdge) <- use $ Graph.userState . Graph.breadcrumbHierarchy . BH.portMapping
            (updates, errors) <- runASTOp $ do
                sidebarUpdates <- (\x y -> [x, y]) <$> GraphBuilder.buildInputSidebarTypecheckUpdate  inEdge
                                                   <*> GraphBuilder.buildOutputSidebarTypecheckUpdate outEdge
                allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
                nodeUpdates <- forM allNodeIds $ GraphBuilder.buildNodeTypecheckUpdate resolveFun
                errors      <- forM allNodeIds $ \nid -> do
                    err <- Error.getError =<< ASTRead.getASTRef nid
                    case err of
                        Nothing -> return Nothing
                        Just e  -> do
                            let toSrcLoc (Target.Method   mod klass method) = APIError.SourceLocation (convertVia @String mod) (Just (convert klass)) (convert method)
                                toSrcLoc (Target.Function mod function)     = APIError.SourceLocation (convertVia @String mod) Nothing (convert function)
                                errorDetails = APIError.CompileErrorDetails (map toSrcLoc (e ^. Error.failedAt)) (map toSrcLoc (e ^. Error.arisingFrom))
                            return $ Just $ (nid, NodeError $ APIError.Error (APIError.CompileError errorDetails) $ e ^. Error.contents)
                return (sidebarUpdates <> nodeUpdates, errors)
            mask_ $ do
                traverse_ (Publisher.notifyNodeTypecheck loc) updates
                for_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0
    Breadcrumb _ -> return ()

updateValues :: GraphLocation
             -> LocalScope
             -> Command Graph [Async ()]
updateValues loc@(GraphLocation path _) scope = do
    childrenMap <- use $ Graph.userState . Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    nodes <- runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> do
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

fileImportPaths :: MonadIO m => FilePath -> m (Map IR.Qualified FilePath)
fileImportPaths file = liftIO $ do
    filePath        <- Path.parseAbsFile file
    currentProjPath <- Package.packageRootForFile filePath
    libs            <- Package.packageImportPaths currentProjPath
    srcs            <- for (snd <$> libs) $ \libPath -> do
        p <- Path.parseAbsDir libPath
        fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources p
    pure $ Map.unions srcs

stop :: Command InterpreterEnv ()
stop = do
    cln     <- use $ Graph.userState . cleanUp
    threads <- use $ Graph.userState . listeners
    Graph.userState . listeners .= []
    liftIO $ mapM_ Async.uninterruptibleCancel threads
    liftIO cln

translate :: Buffer.RedirectMap -> Int -> NodeRef -> NodeRef 
translate redMap off node = wrap $ (unwrap (redMap Map.! (Memory.Ptr $ convert node)))
    `plusPtr` off

primStdModuleName :: IR.Qualified
primStdModuleName = "Std.Primitive"

makePrimStdIfMissing :: Command InterpreterEnv ()
makePrimStdIfMissing = do
    existingStd <- use $ Graph.userState . resolvers . at primStdModuleName
    case existingStd of
        Just _ -> return ()
        Nothing -> do
            (mods, finalizer, typed, computed, ress, units) <- liftScheduler $ do
                (fin, stdUnitRef) <- Std.stdlib @Stage
                lunaroot <- liftIO $ canonicalizePath =<< getEnv Package.lunaRootEnv
                stdPath <- Path.parseAbsDir $ lunaroot <> "/Std/"
                srcs    <- fmap Path.toFilePath . Bimap.toMapR <$> Package.findPackageSources stdPath
                UnitLoader.init
                Scheduler.registerAttr @Unit.UnitRefsMap
                Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton "Std.Primitive" stdUnitRef
                for Std.stdlibImports $ UnitLoader.loadUnit def srcs []
                Unit.UnitRefsMap mods <- Scheduler.getAttr
                units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
                    Unit.Graph r -> UnitMapper.mapUnit n r
                    Unit.Precompiled u -> pure u
                let unitResolvers   = Map.mapWithKey Resolution.resolverFromUnit units
                    importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped (essentialImports <>)
                                                                                                . view Unit.imports <$> mods
                    unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
                (typed, evald) <- ProcessUnits.processUnits def def unitsWithResolvers
                return (mods, fin, typed, evald, unitResolvers, units)
            zoomCommand clsGraph $ runASTOp $ for mods $ \u -> case u ^. Unit.root of
                Unit.Graph r -> IR.deleteSubtree r
                _ -> return ()
            Graph.userState . cleanUp      .= finalizer
            Graph.userState . typedUnits   .= typed
            Graph.userState . mappedUnits  .= units
            Graph.userState . runtimeUnits .= computed
            Graph.userState . resolvers    .= ress

ensureCurrentScope :: Bool -> IR.Qualified -> FilePath -> NodeRef -> Command InterpreterEnv ()
ensureCurrentScope recompute modName path root = do
    modName <- filePathToQualName path
    existing <- use $ Graph.userState . resolvers . at modName
    when (recompute || isNothing existing) $ do
        compileCurrentScope modName path root

compileCurrentScope :: IR.Qualified -> FilePath -> NodeRef -> Command InterpreterEnv ()
compileCurrentScope modName path root = do
    typed   <- use $ Graph.userState . typedUnits
    evald   <- use $ Graph.userState . runtimeUnits
    ress    <- use $ Graph.userState . resolvers

    (mods, newTyped, newEvald, newResolvers, units) <- liftScheduler $ do
        imports <- ImportsPlucker.run root
        UnitLoader.init
        srcs <- fileImportPaths path
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton modName $ Unit.UnitRef (Unit.Graph $ Layout.unsafeRelayout root) (wrap imports)
        for imports $ UnitLoader.loadUnitIfMissing (Set.fromList $ Map.keys ress) srcs [modName]

        Unit.UnitRefsMap mods <- Scheduler.getAttr
        units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
            Unit.Graph r -> UnitMapper.mapUnit n r
            Unit.Precompiled u -> pure u

        let unitResolvers   = Map.union (Map.mapWithKey Resolution.resolverFromUnit units) ress
            importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped (essentialImports <>)
                                                                                        . view Unit.imports <$> mods
            unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
        (newTyped, newEvald) <- ProcessUnits.processUnits typed evald unitsWithResolvers
        return (mods, newTyped, newEvald, unitResolvers, units)
    for mods $ \u -> zoomCommand clsGraph $ runASTOp $ case u ^. Unit.root of
        Unit.Graph r -> when (Layout.relayout r /= root) $ IR.deleteSubtree r
        _ -> return ()

    Graph.userState . mappedUnits  %= Map.union units
    Graph.userState . typedUnits   .= newTyped
    Graph.userState . runtimeUnits .= newEvald
    Graph.userState . resolvers    .= newResolvers

run :: GraphLocation
    -> Graph.ClsGraph
    -> Store.RootedWithRedirects NodeRef
    -> Bool
    -> Bool
    -> Command InterpreterEnv ()
run loc@(GraphLocation file br) clsGraph' rooted' interpret recompute = do
    stop

    let Store.RootedWithRedirects rooted redMap = rooted'
    (root, redirects) <- runASTOp $ Store.deserializeWithRedirects rooted
    let originalCls = clsGraph' ^. Graph.clsClass
        deserializerOff = redirects Map.! someTypeRep @IR.Terms
        newCls = translate redMap deserializerOff originalCls
    let newClsGraph = clsGraph' & BH.refs %~ translate redMap deserializerOff
    Graph.userState . clsGraph .= newClsGraph

    modName <- filePathToQualName file

    makePrimStdIfMissing
    ensureCurrentScope recompute modName file root

    runTC modName loc
    updateNodes loc

    when interpret $ do
        evald <- use $ Graph.userState . runtimeUnits
        asyncs <- case br of
            Breadcrumb (Definition uuid:r) -> zoomCommand clsGraph $ withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb r) $ do
                scope <- runInterpreter file evald
                traverse (updateValues loc) scope
            _ -> return Nothing
        Graph.userState . listeners .= fromMaybe [] asyncs
    zoomCommand clsGraph $ runASTOp $ IR.deleteSubtree root

            -- void $ mask_ $ recomputeCurrentScope imports file
    --         let scopeGetter = if recompute
    --                           then recomputeCurrentScope
    --                           else getCurrentScope
    --         scope  <- mask_ $ scopeGetter imports file
    --         asyncs <- zoom graph $ do
    --             importedModules <- getImportedModules
    --             std             <- liftIO $ readMVar imports
    --             let CompiledModules cmpMods cmpPrims = std
    --                 cmpImportedMods = Map.restrictKeys cmpMods importedModules
    --                 visibleModules = CompiledModules cmpImportedMods cmpPrims
    --                 flatVisibleScope = flattenScope $ Scope visibleModules
    --                 moduleEnv      = unionImports flatVisibleScope scope
    --             modName <- filePathToQualName file
    --             funName <- preuse $
    --                 Graph.clsFuns . at uuid . _Just . Graph.funName
    --                     let functionName = fromMaybe "unknown function" funName
    --                     runTC modName functionName moduleEnv
    --                     {-updateMonads loc-}
    --                     if interpret then do
    --                         scope  <- runInterpreter file moduleEnv
    --                         traverse (updateValues loc) scope
    --                     else return Nothing
    --         listeners .= fromMaybe [] asyncs
