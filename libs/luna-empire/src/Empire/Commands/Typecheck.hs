{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.Typecheck where

import           Control.Arrow                    ((***), (&&&))
import           Control.Concurrent.Async         (Async)
import qualified Control.Concurrent.Async         as Async
import           Control.Concurrent               (MVar, readMVar)
import qualified Control.Concurrent.MVar.Lifted   as Lifted
import           Control.Exception.Safe           (mask_)
import           Control.Monad                    (void)
import           Control.Monad.Except             hiding (when)
import           Control.Monad.Reader             (ask, runReaderT)
import           Control.Monad.State.Strict       (execStateT)
import qualified Data.IORef                       as IORef
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, maybeToList)
import           Empire.Prelude                   hiding (mapping, toList)
import           System.Directory                 (canonicalizePath, withCurrentDirectory)
import           System.Environment               (getEnv)
import           System.FilePath                  (takeDirectory)
import qualified Path

import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem(Definition))
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import qualified LunaStudio.Data.Error            as APIError
import           LunaStudio.Data.NodeValue        (NodeValue (..))
import           LunaStudio.Data.Visualization    (VisualizationValue (..))

import           Empire.ASTOp                     (getImportedModules, runASTOp, runTypecheck, runModuleTypecheck)
import qualified Empire.ASTOps.Read               as ASTRead
import           Empire.Commands.Breadcrumb       (runInternalBreadcrumb, withRootedFunction)
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Empire

import           Luna.Builtin.Data.LunaEff        (runError, runIO)
import           Luna.Builtin.Data.Module         (Imports (..), unionImports, unionsImports)
import           Luna.Builtin.Prim                (SingleRep (..), ValueRep (..), getReps)
import qualified Luna.Compilation                 as Compilation
import qualified Luna.Project                     as Project
import           Luna.Compilation                 (CompiledModules (..))
import qualified Luna.IR                          as IR
import           Luna.Pass.Data.ExprMapping
import qualified Luna.Pass.Evaluation.Interpreter as Interpreter
import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget(TgtDef))
import qualified Luna.IR.Layer.Errors             as Errors
import           OCI.IR.Name.Qualified            (QualName)

runTC :: QualName -> String -> Imports -> Command Graph ()
runTC moduleName functionName imports = do
    let currentTarget = TgtDef (convert moduleName) (convert functionName)
    runTypecheck currentTarget imports
    runASTOp $ do
        mapping <- unwrap <$> IR.getAttr @ExprMapping
        Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    return ()

withProjectCurrentDirectory :: FilePath -> IO a -> IO a
withProjectCurrentDirectory currentFile act = do
    rootPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile currentFile
    let projectDirectory = maybe (takeDirectory currentFile) Path.toFilePath rootPath
    withCurrentDirectory projectDirectory act

runInterpreter :: FilePath -> Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter path imports = runASTOp $ do
    selfRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    IR.matchExpr selfRef $ \case
        IR.ASGFunction _ [] b -> do
            bodyRef <- IR.source b
            res     <- Interpreter.interpret' imports . IR.unsafeGeneralize $ bodyRef
            mask_ $ liftIO $ do
                ref <- IORef.newIORef def
                withProjectCurrentDirectory path
                    $ runIO $ runError $ execStateT (Interpreter.unScopeT res) ref
                Just <$> IORef.readIORef ref
        _ -> return Nothing

updateNodes :: GraphLocation -> Command Graph ()
updateNodes loc@(GraphLocation _ br) = do
    (inEdge, outEdge) <- use $ Graph.breadcrumbHierarchy . BH.portMapping
    (updates, errors) <- runASTOp $ do
        sidebarUpdates <- (\x y -> [x, y]) <$> GraphBuilder.buildInputSidebarTypecheckUpdate  inEdge
                                           <*> GraphBuilder.buildOutputSidebarTypecheckUpdate outEdge
        allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
        nodeUpdates <- mapM GraphBuilder.buildNodeTypecheckUpdate allNodeIds
        errors      <- forM allNodeIds $ \nid -> do
            errs <- IR.getLayer @IR.Errors =<< ASTRead.getASTRef nid
            case errs of
                []     -> return Nothing
                e : es -> do
                    let toSrcLoc (Errors.ModuleTagged mod (Errors.FromMethod klass method)) = APIError.SourceLocation (convert mod) (Just (convert klass)) (convert method)
                        toSrcLoc (Errors.ModuleTagged mod (Errors.FromFunction function))   = APIError.SourceLocation (convert mod) Nothing (convert function)
                        errorDetails = APIError.CompileErrorDetails (map toSrcLoc (e ^. Errors.arisingFrom)) (map toSrcLoc (e ^. Errors.requiredBy))
                    return $ Just $ (nid, NodeError $ APIError.Error (APIError.CompileError errorDetails) $ e ^. Errors.description)
        return (sidebarUpdates <> nodeUpdates, errors)
    mask_ $ do
        traverse_ (Publisher.notifyNodeTypecheck loc) updates
        for_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc@(GraphLocation _ br) = return ()
    --zoom graph $ zoomBreadcrumb br $ do
        {-newMonads <- runASTOp GraphBuilder.buildMonads-}
        {-Publisher.notifyMonadsUpdate loc newMonads-}

updateValues :: GraphLocation
             -> Interpreter.LocalScope
             -> Command Graph [Async ()]
updateValues loc@(GraphLocation path _) scope = do
    childrenMap <- use $ Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    allVars <- runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> do
        pointer <- ASTRead.getASTPointer nid
        IR.matchExpr pointer $ \case
            IR.Unify{} -> Just . (nid,) <$> ASTRead.getVarNode pointer
            _          -> return Nothing
    let send nid m = flip runReaderT env $
            Publisher.notifyResultUpdate loc nid m 0
        sendRep nid (ErrorRep e)     = send nid
                                     $ NodeError
                                     $ APIError.Error APIError.RuntimeError
                                     $ convert e
        sendRep nid (SuccessRep s l) = send nid
                                     $ NodeValue (convert s)
                                     $ Value . convert <$> l
        sendStreamRep nid a@(ErrorRep _)   = sendRep nid a
        sendStreamRep nid (SuccessRep s l) = send nid
                                           $ NodeValue (convert s)
                                           $ StreamDataPoint . convert <$> l
    asyncs <- liftIO $ withProjectCurrentDirectory path $
        forM allVars $ \(nid, ref) -> do
            let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
            forM resVal $ \v -> do
                value <- getReps v
                case value of
                    OneTime r   -> Async.async $ sendRep nid r
                    Streaming f -> do
                        send nid (NodeValue "Stream" $ Just StreamStart)
                        Async.async (f (sendStreamRep nid))
    return $ catMaybes asyncs

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

newtype Scope = Scope CompiledModules
makeWrapped ''Scope

flattenScope :: Scope -> Imports
flattenScope (Scope (CompiledModules mods prims)) =
    unionsImports $ prims : Map.elems mods

createStdlib :: String -> IO (IO (), Scope)
createStdlib =
    fmap (id *** Scope) . Compilation.prepareStdlib . Map.singleton "Std"

filePathToQualName :: MonadIO m => FilePath -> m QualName
filePathToQualName path = liftIO $ do
    path' <- Path.parseAbsFile path
    root  <- Project.projectRootForFile path'
    let projName = Project.getProjectName root
    file  <- Path.stripProperPrefix (root Path.</> $(Path.mkRelDir "src")) path'
    return $ Project.mkQualName projName file

recomputeCurrentScope :: MVar CompiledModules
                      -> FilePath
                      -> Command InterpreterEnv Imports
recomputeCurrentScope imports file = do
    Lifted.modifyMVar imports $ \imps -> do
        importPaths     <- liftIO $ do
            filePath        <- Path.parseAbsFile file
            currentProjPath <- Project.projectRootForFile filePath
            Project.projectImportPaths currentProjPath
        qualName        <- filePathToQualName file
        (f, nimps)      <- zoom graph $ do
            t <- runModuleTypecheck qualName (Map.fromList importPaths) imps
            case t of
                Right (a, b) -> return (a, b)
                Left e  -> error $ show e <> " " <> file
        let nimpsF = nimps & Compilation.modules . at qualName ?~ f
        return (nimpsF, f)

getCurrentScope :: MVar CompiledModules
                -> FilePath
                -> Command InterpreterEnv Imports
getCurrentScope imports file = do
    fs       <- liftIO $ readMVar imports
    qualName <- filePathToQualName file
    case fs ^. Compilation.modules . at qualName of
        Just f -> return f
        _      -> recomputeCurrentScope imports file

stop :: Command InterpreterEnv ()
stop = do
    cln       <- use cleanUp
    threads   <- use listeners
    listeners .= []
    liftIO $ mapM_ Async.uninterruptibleCancel threads
    liftIO cln

run :: MVar CompiledModules
    -> GraphLocation
    -> Bool
    -> Bool
    -> Command InterpreterEnv ()
run imports loc@(GraphLocation file br) interpret recompute = do
    case br of
        Breadcrumb [] -> do
            void $ mask_ $ recomputeCurrentScope imports file
        Breadcrumb (Definition uuid:rest) -> do
            let scopeGetter = if recompute
                              then recomputeCurrentScope
                              else getCurrentScope
            scope  <- mask_ $ scopeGetter imports file
            asyncs <- zoom graph $ do
                importedModules <- getImportedModules
                std             <- liftIO $ readMVar imports
                let CompiledModules cmpMods cmpPrims = std
                    cmpImportedMods = Map.restrictKeys cmpMods importedModules
                    visibleModules = CompiledModules cmpImportedMods cmpPrims
                    flatVisibleScope = flattenScope $ Scope visibleModules
                    moduleEnv      = unionImports flatVisibleScope scope
                modName <- filePathToQualName file
                funName <- preuse $
                    Graph.clsFuns . at uuid . _Just . Graph.funName
                withRootedFunction uuid $ do
                    runInternalBreadcrumb (Breadcrumb rest) $ do
                        let functionName = fromMaybe "unknown function" funName
                        runTC modName functionName moduleEnv
                        updateNodes  loc
                        {-updateMonads loc-}
                        if interpret then do
                            scope  <- runInterpreter file moduleEnv
                            traverse (updateValues loc) scope
                        else return Nothing
            listeners .= fromMaybe [] asyncs
