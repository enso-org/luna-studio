{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Empire.Commands.Typecheck where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Except              hiding (when)
import           Control.Monad.Reader              (ask, runReaderT)
import           Control.Monad.State               (execStateT, gets)
import           Data.List                         (sort)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isNothing, maybeToList)
import           Empire.Prelude                    hiding (toList)
import           Prologue                          (catMaybes, fromString, itoListOf, itraverse, toListOf)

import qualified LunaStudio.Data.Error             as APIError
import           LunaStudio.Data.GraphLocation     (GraphLocation (..))
import           LunaStudio.Data.MonadPath         (MonadPath (MonadPath))
import           LunaStudio.Data.Node              (NodeId, nodeId)
import qualified LunaStudio.Data.NodeMeta          as NodeMeta
import           LunaStudio.Data.NodeValue         (NodeValue(..), VisualizationValue (JsonValue))
import           LunaStudio.Data.TypeRep           (TypeRep (TCons))
import           LunaStudio.Data.PortDefault       (PortValue (StringValue))

import           Empire.ASTOp                      (EmpirePass, runASTOp, runTypecheck)
import qualified Empire.ASTOps.Read                as ASTRead
import qualified Empire.Commands.AST               as AST
import           Empire.Commands.Breadcrumb        (zoomBreadcrumb)
import qualified Empire.Commands.GraphBuilder      as GraphBuilder
import qualified Empire.Commands.GraphUtils        as GraphUtils
import qualified Empire.Commands.Publisher         as Publisher
import           Empire.Data.AST                   (NodeRef)
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy   as BH
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph
import           Empire.Empire

import           Luna.Builtin.Data.LunaValue       (LunaData, listenReps)
import           Luna.Builtin.Data.LunaEff         (runIO, runError)
import           Luna.Builtin.Data.Module          (Imports (..), importedClasses, importedFunctions)
import           Luna.Builtin.Data.Class           (Class (..))
import qualified Luna.Builtin.Std                  as Std
import qualified Luna.IR                           as IR
import qualified Luna.Pass.Evaluation.Interpreter  as Interpreter
import qualified Luna.Pass.Typechecking.Typecheck  as Typecheck
import qualified OCI.IR.Combinators                as IR
import           OCI.Pass                          (SubPass)
import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget (TgtNone))
import           Luna.Pass.Data.ExprMapping
import qualified Luna.Compilation                  as Compilation


import System.IO.Unsafe
import Data.IORef

runTC :: Imports -> Command Graph ()
runTC imports = do
    runTypecheck imports
    runASTOp $ do
        mapping <- unwrap <$> IR.getAttr @ExprMapping
        Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    return ()

runInterpreter :: Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter imports = runASTOp $ do
    bodyRef    <- preuse $ Graph.breadcrumbHierarchy . BH.body
    res        <- mapM (Interpreter.interpret' imports . IR.unsafeGeneralize) bodyRef
    case res of
        Nothing -> return Nothing
        Just v  -> do
            result <- liftIO $ runIO $ runError $ execStateT v def
            case result of
                Left e  -> return Nothing
                Right r -> return $ Just r

reportError :: GraphLocation -> NodeId -> Maybe APIError.Error -> Command InterpreterEnv ()
reportError loc nid err = do
    cachedErr <- uses errorsCache $ Map.lookup nid
    when (cachedErr /= err) $ do
        errorsCache %= Map.alter (const err) nid
        valuesCache %= Map.delete nid
        case err of
            Just e  -> Publisher.notifyResultUpdate loc nid (NodeError e)     0
            Nothing -> Publisher.notifyResultUpdate loc nid (NodeValue "" []) 0

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc@(GraphLocation _ br) = zoom graph $ zoomBreadcrumb br $ do
    portMapping <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
    (updates, errors) <- runASTOp $ do
        sidebarUpdates <- case portMapping of
            Just (i, o) -> do
                (u1, u2) <- (,) <$> GraphBuilder.buildInputSidebarTypecheckUpdate i <*> GraphBuilder.buildOutputSidebarTypecheckUpdate o
                return [u1, u2]
            Nothing     -> return []
        allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
        nodeUpdates <- mapM GraphBuilder.buildNodeTypecheckUpdate allNodeIds
        errors      <- forM allNodeIds $ \nid -> do
            errs <- IR.getLayer @IR.Errors =<< ASTRead.getASTPointer nid
            case errs of
                []     -> return Nothing
                e : es -> return $ Just $ (nid, NodeError $ APIError.Error APIError.CompileError e)
        return (sidebarUpdates ++ nodeUpdates, errors)
    mapM_ (Publisher.notifyNodeTypecheck loc) updates
    forM_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc@(GraphLocation _ br) = return ()--zoom graph $ zoomBreadcrumb br $ do
    {-newMonads <- runASTOp GraphBuilder.buildMonads-}
    {-Publisher.notifyMonadsUpdate loc newMonads-}

updateValues :: GraphLocation -> Interpreter.LocalScope -> Command InterpreterEnv ()
updateValues loc scope = do
    childrenMap <- use $ graph . Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env     <- ask
    allVars <- zoom graph $ runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> case tgt of
        BH.MatchNode r -> Just . (nid,) <$> ASTRead.getVarNode r
        _              -> return Nothing
    forM_ allVars $ \(nid, ref) -> do
        let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
        liftIO $ forM_ resVal $ \v -> listenReps v $ \case
            Left  err            -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeError $ APIError.Error APIError.RuntimeError $ convert err) 0
            Right (short, longs) -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeValue (fromString short) $ JsonValue <$> longs) 0

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

newtype Scope = Scope Imports

createStdlib :: String -> IO Scope
createStdlib = fmap Scope . Compilation.createStdlib

getSymbolMap :: Scope -> SymbolMap
getSymbolMap (Scope (Imports clss funcs)) = SymbolMap (convert <$> Map.keys funcs) classes where
    classes = processClass <$> Map.mapKeys convert clss
    processClass (Class conses methods) = convert <$> Map.keys methods

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    std <- use imports
    liftIO $ print $ Map.keys $ std ^. importedClasses
    zoom graph $ runTC std
    updateNodes  loc
    {-updateMonads loc-}
    scope <- zoom graph $ runInterpreter std
    mapM_ (updateValues loc) scope
