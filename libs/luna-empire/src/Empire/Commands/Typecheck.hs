{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}

module Empire.Commands.Typecheck where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Reader              (ask, runReaderT)
import           Control.Monad.State               (execStateT, gets)
import           Control.Monad.Except
import           Data.List                         (sort)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isNothing)
import           Empire.Prelude
import           Prologue                          (itoListOf, itraverse, toListOf, fromString)

import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import           Empire.API.Data.MonadPath         (MonadPath (MonadPath))
import           Empire.API.Data.Node              (NodeId, nodeId)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.TypeRep           (TypeRep (TCons))
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult
import           Empire.ASTOp                      (EmpirePass, runASTOp)
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphBuilder      as GraphBuilder
import qualified Empire.Commands.GraphUtils        as GraphUtils
import qualified Empire.Commands.Publisher         as Publisher
import qualified Empire.ASTOps.Read                as ASTRead
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy   as BH
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph
import           Empire.Data.AST                   (NodeRef)
import           Empire.Empire

import qualified Luna.Builtin.Std                  as Std
import qualified Luna.IR                           as IR
import           Luna.Builtin.Data.Module          (Imports (..), Module (..))
import           Luna.Builtin.Data.LunaValue       (listenShortRep, LunaData)
import qualified Luna.Pass.Typechecking.Typecheck  as Typecheck
import qualified Luna.Pass.Evaluation.Interpreter  as Interpreter
import qualified OCI.IR.Combinators                as IR
import           OCI.Pass                          (SubPass)


runTC :: Imports -> Command Graph ()
runTC imports = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    runASTOp $ do
        roots   <- mapM GraphUtils.getASTPointer allNodeIds
        Typecheck.typecheck imports $ map IR.unsafeGeneralize roots
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
            Just e  -> Publisher.notifyResultUpdate loc nid (NodeResult.Error e)     0
            Nothing -> Publisher.notifyResultUpdate loc nid (NodeResult.Value "" []) 0

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc = do
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    forM_ allNodeIds $ \nid -> do
        err <- zoom graph $ runASTOp $ do
            ref <- GraphUtils.getASTTarget nid
            AST.getError ref
        reportError loc nid err

        rep <- zoom graph $ runASTOp $ GraphBuilder.buildNodeTypecheckUpdate nid
        Publisher.notifyNodeTypecheck loc rep
        -- FIXME[MM]: use cache? maybe it's not required any more
        -- cached <- uses nodesCache $ Map.lookup nid
        -- when (cached /= Just rep) $ do
            -- nodesCache %= Map.insert nid rep
    edgeNodes  <- zoom graph $ runASTOp $ GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildEdgeNodes c
    forM_ edgeNodes $ \edges -> forM_ edges $ \rep -> do
        let nid = rep ^. nodeId
        cached <- uses nodesCache $ Map.lookup nid
        when (cached /= Just rep) $ do
            Publisher.notifyNodeUpdate loc rep
            nodesCache %= Map.insert nid rep

updateMonads :: GraphLocation -> Command InterpreterEnv ()
updateMonads loc = do
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    let monad1 = MonadPath (TCons "MonadMock1" []) (sort allNodeIds) --FIXME[pm] provide real data
        monad2 = MonadPath (TCons "MonadMock2" []) allNodeIds
    Publisher.notifyMonadsUpdate loc [monad1, monad2]

updateValues :: GraphLocation -> Interpreter.LocalScope -> Command InterpreterEnv ()
updateValues loc scope = do
    childrenMap <- use $ graph . Graph.breadcrumbHierarchy . BH.children
    let allNodes = Map.assocs $ view BH.self <$> childrenMap
    env        <- ask
    forM_ allNodes $ \(nid, tgt) -> do
        case tgt of
            BH.MatchNode r -> do
                ref <- zoom graph $ runASTOp $ ASTRead.getVarNode r
                let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
                liftIO $ forM_ resVal $ \v -> listenShortRep v $ \case
                    Left  err    -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeResult.Error $ APIError.RuntimeError err) 0
                    Right strRep -> flip runReaderT env $ Publisher.notifyResultUpdate loc nid (NodeResult.Value (fromString strRep) []) 0

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    std <- liftIO $ Std.stdlib
    zoom graph $ runTC std
    updateNodes loc
    updateMonads loc
    scope <- zoom graph $ runInterpreter std
    mapM_ (updateValues loc) scope
