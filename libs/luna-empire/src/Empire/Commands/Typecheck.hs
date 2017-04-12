{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Empire.Commands.Typecheck where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Except
import           Control.Monad.Reader              (ask, runReaderT)
import           Control.Monad.State               (execStateT, gets)
import           Data.List                         (sort)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isNothing, maybeToList)
import           Empire.Prelude
import           Prologue                          (catMaybes, fromString, itoListOf, itraverse, toListOf)

import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import           Empire.API.Data.MonadPath         (MonadPath (MonadPath))
import           Empire.API.Data.Node              (NodeId, nodeId)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.TypeRep           (TypeRep (TCons))
import           Empire.API.Data.PortDefault       (PortValue (StringValue))
import           Empire.API.Graph.NodeResultUpdate (NodeValue(..))
import           Empire.ASTOp                      (EmpirePass, runASTOp)
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
import           Luna.Builtin.Data.Module          (Imports (..), Module (..))
import qualified Luna.Builtin.Std                  as Std
import qualified Luna.IR                           as IR
import qualified Luna.Pass.Evaluation.Interpreter  as Interpreter
import qualified Luna.Pass.Typechecking.Typecheck  as Typecheck
import qualified OCI.IR.Combinators                as IR
import           OCI.Pass                          (SubPass)
import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget (TgtNone))

import System.IO.Unsafe
import Data.IORef

runTC :: Imports -> Command Graph ()
runTC imports = do
    root <- preuse $ Graph.breadcrumbHierarchy . BH.body
    runASTOp $ Typecheck.typecheck TgtNone imports $ map IR.unsafeGeneralize $ maybeToList root
    return ()

runInterpreter :: Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter imports = runASTOp $ do
    bodyRef    <- preuse $ Graph.breadcrumbHierarchy . BH.body
    res        <- mapM (Interpreter.interpret' imports . IR.unsafeGeneralize) bodyRef
    case res of
        Nothing -> return Nothing
        Just v  -> do
            result <- liftIO $ runExceptT $ execStateT v def
            case result of
                Left e  -> return Nothing
                Right r -> return $ Just r

reportError :: GraphLocation -> NodeId -> Maybe (APIError.Error TypeRep) -> Command InterpreterEnv ()
reportError loc nid err = do
    cachedErr <- uses errorsCache $ Map.lookup nid
    when (cachedErr /= err) $ do
        errorsCache %= Map.alter (const err) nid
        valuesCache %= Map.delete nid
        case err of
            Just e  -> Publisher.notifyResultUpdate loc nid (NodeError e)     0
            Nothing -> Publisher.notifyResultUpdate loc nid (NodeValue "" []) 0

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc@(GraphLocation _ _ br) = zoom graph $ zoomBreadcrumb br $ do
    portMapping <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
    updates <- runASTOp $ do
        sidebarUpdates <- case portMapping of
            Just (i, o) -> do
                (u1, u2) <- (,) <$> GraphBuilder.buildInputEdgeTypecheckUpdate i <*> GraphBuilder.buildOutputEdgeTypecheckUpdate o
                return [u1, u2]
            Nothing     -> return []
        allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
        nodeUpdates <- mapM GraphBuilder.buildNodeTypecheckUpdate allNodeIds
        return $ sidebarUpdates ++ nodeUpdates
    mapM_ (Publisher.notifyNodeTypecheck loc) updates

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc@(GraphLocation _ _ br) = zoom graph $ zoomBreadcrumb br $ do
    newMonads <- runASTOp GraphBuilder.buildMonads
    Publisher.notifyMonadsUpdate loc newMonads

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
            Left  err            -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeError $ APIError.RuntimeError err) 0
            Right (short, longs) -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeValue (fromString short) $ StringValue <$> longs) 0

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

-- temporary: at some point this will be part of our state
std :: (Std.WorldState, Imports)
std = unsafePerformIO Std.mockStdlib
{-# NOINLINE std #-}

flushWorld :: IO ()
flushWorld = let Std.WorldState c s = fst std in writeIORef c 1 >> writeIORef s []

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    zoom graph $ runTC $ snd std
    updateNodes  loc
    updateMonads loc
    liftIO flushWorld
    scope <- zoom graph $ runInterpreter $ snd std
    mapM_ (updateValues loc) scope
