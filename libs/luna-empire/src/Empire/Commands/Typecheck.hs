{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Empire.Commands.Typecheck where

import           Control.Arrow                    ((***), (&&&))
import           Control.Concurrent               (forkIO, killThread)
import           Control.Monad                    (void)
import           Control.Monad.Except             hiding (when)
import           Control.Monad.Reader             (ask, runReaderT)
import           Control.Monad.State              (execStateT)
import qualified Data.Map                         as Map
import           Data.Maybe                       (maybeToList)
import           Empire.Prelude                   hiding (toList)
import           Prologue                         (catMaybes)
import           System.Directory                 (withCurrentDirectory)
import           System.FilePath                  (takeDirectory)

import qualified LunaStudio.Data.Error            as APIError
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import           LunaStudio.Data.Node             (NodeId)
import           LunaStudio.Data.NodeValue        (NodeValue (..), VisualizationValue (..))
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..))

import           Empire.ASTOp                     (runASTOp, runTypecheck, runModuleTypecheck)
import qualified Empire.ASTOps.Read               as ASTRead
import           Empire.Commands.Breadcrumb       (zoomBreadcrumb')
import           Empire.Commands.AST              as AST
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Empire

import           Luna.Builtin.Data.Class          (Class (..))
import           Luna.Builtin.Data.LunaEff        (runError, runIO)
import           Luna.Builtin.Data.Module         (Imports (..), importedClasses, unionImports, unionsImports)
import           Luna.Builtin.Prim                (SingleRep (..), ValueRep (..), getReps)
import qualified Luna.Compilation                 as Compilation
import qualified Luna.Project                     as Project
import           Luna.Compilation                 (CompiledModules (..))
import qualified Luna.IR                          as IR
import           Luna.Pass.Data.ExprMapping
import qualified Luna.Pass.Evaluation.Interpreter as Interpreter
import qualified Luna.IR.Layer.Errors             as Errors

import           System.Directory                     (canonicalizePath)
import           System.Environment                   (getEnv)
import qualified Path

runTC :: Imports -> Command Graph ()
runTC imports = do
    runTypecheck imports
    runASTOp $ do
        mapping <- unwrap <$> IR.getAttr @ExprMapping
        Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    return ()

runInterpreter :: FilePath -> Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter path imports = runASTOp $ do
    rootPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile path
    selfRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    IR.matchExpr selfRef $ \case
        IR.ASGFunction _ [] b -> do
            bodyRef <- IR.source b
            res     <- Interpreter.interpret' imports . IR.unsafeGeneralize $ bodyRef
            result  <- liftIO $ withCurrentDirectory (maybe (takeDirectory path) Path.toFilePath rootPath) $ runIO $ runError $ execStateT res def
            case result of
                Left e  -> return Nothing
                Right r -> return $ Just r
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
                 e : es -> return $ Just $ (nid, NodeError $ APIError.Error APIError.CompileError $ e ^. Errors.description)
         return (sidebarUpdates <> nodeUpdates, errors)
     traverse_ (Publisher.notifyNodeTypecheck loc) updates
     for_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc@(GraphLocation _ br) = return ()--zoom graph $ zoomBreadcrumb br $ do
    {-newMonads <- runASTOp GraphBuilder.buildMonads-}
    {-Publisher.notifyMonadsUpdate loc newMonads-}

updateValues :: GraphLocation -> Interpreter.LocalScope -> Command Graph ()
updateValues loc scope = do
    childrenMap <- use $ Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    allVars <- runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> do
        pointer <- ASTRead.getASTPointer nid
        IR.matchExpr pointer $ \case
            IR.Unify{} -> Just . (nid,) <$> ASTRead.getVarNode pointer
            _          -> return Nothing
    for_ allVars $ \(nid, ref) -> do
        let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
            send m = flip runReaderT env $ Publisher.notifyResultUpdate loc nid m 0
            sendRep (ErrorRep e)     = send $ NodeError $ APIError.Error APIError.RuntimeError $ convert e
            sendRep (SuccessRep s l) = send $ NodeValue (convert s) $ Value . convert <$> l
            sendStreamRep a@(ErrorRep _)   = sendRep a
            sendStreamRep (SuccessRep s l) = send $ NodeValue (convert s) $ StreamDataPoint . convert <$> l
        liftIO $ for_ resVal $ \v -> do
            value <- getReps v
            case value of
                OneTime r   -> sendRep r
                Streaming f -> do
                    send (NodeValue "Stream" $ Just StreamStart)
                    void $ forkIO $ f sendStreamRep

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

newtype Scope = Scope CompiledModules
makeWrapped ''Scope

flattenScope :: Scope -> Imports
flattenScope (Scope (CompiledModules mods prims)) = unionsImports $ prims : Map.elems mods

createStdlib :: String -> IO (IO (), Scope)
createStdlib = fmap (id *** Scope) . Compilation.prepareStdlib . Map.singleton "Std"

getSymbolMap :: Scope -> SymbolMap
getSymbolMap (flattenScope -> Imports clss funcs) = SymbolMap functions classes where
    functions = conses <> (convert <$> Map.keys funcs)
    conses    = getConses =<< Map.elems clss
    getConses (Class conses _) = convert <$> Map.keys conses
    classes   = processClass <$> Map.mapKeys convert clss
    processClass (Class _ methods) = convert <$> Map.keys methods

recomputeCurrentScope :: FilePath -> Command InterpreterEnv Imports
recomputeCurrentScope file = do
    imps        <- use imports
    lunaroot    <- liftIO $ canonicalizePath =<< getEnv "LUNAROOT"
    currentProjPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile file
    let importPaths = ("Std", lunaroot <> "/Std/") : ((Project.getProjectName &&& Path.toFilePath) <$> maybeToList currentProjPath)
    (f, nimps) <- zoom graph $ runModuleTypecheck (Map.fromList importPaths) imps
    fileScope ?= f
    imports   .= nimps
    return f

getCurrentScope :: FilePath -> Command InterpreterEnv Imports
getCurrentScope file = do
    fs   <- use fileScope
    case fs of
        Just f -> return f
        _      -> recomputeCurrentScope file

run :: GraphLocation -> Command InterpreterEnv ()
run loc@(GraphLocation file br) = do
    std        <- use imports
    cln        <- use cleanUp
    threads    <- use listeners
    scope      <- getCurrentScope file
    let imps = unionImports (flattenScope $ Scope std) scope
    listeners .= []
    case br of
        Breadcrumb [] -> void $ recomputeCurrentScope file
        _             -> zoom graph $ flip (zoomBreadcrumb' br) (return ()) $ do
            runTC imps
            updateNodes  loc
            {-updateMonads loc-}
            liftIO cln
            liftIO $ mapM killThread threads
            scope <- runInterpreter file imps
            traverse_ (updateValues loc) scope
