{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Empire.Commands.Typecheck where

import           Control.Monad                     (forM_, void)
import           Control.Monad.Reader              (ask)
import           Data.List                         (sort)
import qualified Data.Map                          as Map
import           Data.Maybe                        (isNothing)
import           Empire.Prelude

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
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph
import           Empire.Empire

import qualified Luna.Builtin.Std                  as Std
import qualified Luna.IR                           as IR
import qualified Luna.IR.Term.Function             as IR.Function
import           Luna.IR.Term.Unit                 (Imports (..), Module (..))
import qualified Luna.Pass.Typechecking.Typecheck  as Typecheck
import qualified OCI.IR.Combinators                as IR
import           OCI.Pass                          (SubPass)


getNodeValueReprs :: NodeId -> Command Graph (Either String a)
getNodeValueReprs nid = runASTOp $ do
    nodeRef <- GraphUtils.getASTPointer nid
    metaMay <- AST.readMeta nodeRef
    $notImplemented

collect :: Monad m => a -> m ()
collect _ = return ()
    {-putStrLn $ "After pass: " <> pass-}
    {-st <- TypeCheckState.get-}
    {-putStrLn $ "State is: " <> show st-}

runTC :: Command Graph ()
runTC = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    runASTOp $ do
        roots   <- mapM GraphUtils.getASTPointer allNodeIds
        imports <- liftIO Std.stdlib
        Typecheck.typecheck imports $ map IR.unsafeGeneralize roots
    return ()

runInterpreter :: Command Graph ()
runInterpreter = runASTOp $ do
    _ast       <- use Graph.ast
    allNodes   <- uses Graph.breadcrumbHierarchy topLevelIDs
    refs       <- mapM GraphUtils.getASTPointer allNodes
    metas      <- mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    _evals     <- mapM GraphUtils.getASTVar sorted
    newAst     <- liftIO $ fmap snd $ $notImplemented
    Graph.ast .= newAst
    return ()

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

updateValues :: GraphLocation -> Command InterpreterEnv ()
updateValues loc = do
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    forM_ allNodeIds $ \nid -> Publisher.notifyResultUpdate loc nid (NodeResult.Value "Hello!" []) 0
    {-dests <- use destructors-}
    {-liftIO $ sequence_ dests-}
    {-destructors .= []-}
    {-allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs-}
    {-forM_ allNodeIds $ \nid -> do-}
        {-noErrors <- isNothing <$> uses errorsCache (Map.lookup nid)-}
        {-when noErrors $ do-}
            {-val <- zoom graph $ getNodeValueReprs nid-}
            {-$notImplemented-}

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    zoom graph runTC
    updateNodes loc
    updateMonads loc
    -- zoom graph runInterpreter
    updateValues loc
