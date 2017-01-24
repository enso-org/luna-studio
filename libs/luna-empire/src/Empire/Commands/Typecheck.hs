{-# LANGUAGE OverloadedStrings #-}

module Empire.Commands.Typecheck where

import           Empire.Prelude
import           Control.Monad           (forM_, void)
import           Control.Monad.Reader    (ask)
import           Data.List               (sort)
import qualified Data.Map                as Map
import           Data.Maybe              (isNothing)

import qualified Empire.Data.Graph                 as Graph
import           Empire.Data.Graph                 (Graph)
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import           Empire.API.Data.Node              (NodeId, nodeId)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult
import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.ASTOp                      (runASTOp)
import           Empire.Empire
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import qualified Empire.Commands.GraphBuilder      as GraphBuilder
import qualified Empire.Commands.Publisher         as Publisher



getNodeValueReprs :: NodeId -> Command Graph (Either String AST.ValueRep)
getNodeValueReprs nid = runASTOp $ do
    nodeRef <- GraphUtils.getASTPointer nid
    metaMay <- AST.readMeta nodeRef
    case metaMay of
        Just meta -> if meta ^. NodeMeta.displayResult
            then do
                valRef <- GraphUtils.getASTVar nid
                AST.getNodeValue valRef
            else   return $ Right $ AST.PlainVal ("", [])
        Nothing -> return $ Right $ AST.PlainVal ("", [])

collect :: Monad m => a -> m ()
collect _ = return ()
    {-putStrLn $ "After pass: " <> pass-}
    {-st <- TypeCheckState.get-}
    {-putStrLn $ "State is: " <> show st-}

runTC :: Command Graph ()
runTC = do
    allNodeIds <- uses Graph.nodeMapping $ Map.keys
    _roots <- runASTOp $ mapM GraphUtils.getASTPointer allNodeIds
    $notImplemented
    return ()

runInterpreter :: Command Graph ()
runInterpreter = runASTOp $ do
    _ast        <- use Graph.ast
    allNodes   <- uses Graph.breadcrumbHierarchy topLevelIDs
    refs       <- mapM GraphUtils.getASTPointer allNodes
    metas      <- mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    _evals      <- mapM GraphUtils.getASTVar sorted
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

        rep <- zoom graph $ runASTOp $ GraphBuilder.buildNode nid
        cached <- uses nodesCache $ Map.lookup nid
        when (cached /= Just rep) $ do
            Publisher.notifyNodeUpdate loc rep
            nodesCache %= Map.insert nid rep
    edgeNodes  <- zoom graph $ runASTOp $ GraphBuilder.buildEdgeNodes
    forM_ edgeNodes $ \edges -> forM_ edges $ \rep -> do
        let nid = rep ^. nodeId
        cached <- uses nodesCache $ Map.lookup nid
        when (cached /= Just rep) $ do
            Publisher.notifyNodeUpdate loc rep
            nodesCache %= Map.insert nid rep

updateValues :: GraphLocation -> Command InterpreterEnv ()
updateValues loc = do
    dests <- use destructors
    liftIO $ sequence_ dests
    destructors .= []
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    forM_ allNodeIds $ \nid -> do
        noErrors <- isNothing <$> uses errorsCache (Map.lookup nid)
        when noErrors $ do
            val <- zoom graph $ getNodeValueReprs nid
            case val of
                Left err -> reportError loc nid $ Just $ APIError.RuntimeError err
                Right v  -> case v of
                        AST.PlainVal (name, val') -> do
                            cached <- uses valuesCache $ Map.lookup nid
                            when (cached /= Just val') $ do
                                Publisher.notifyResultUpdate loc nid (NodeResult.Value name val') 100
                                valuesCache %= Map.insert nid val'
                        AST.Listener lst -> do
                            commEnv <- ask
                            destructor <- liftIO $ lst $ \(name, val') -> void $ runEmpire commEnv () $ Publisher.notifyResultUpdate loc nid (NodeResult.Value name val') 100
                            destructors %= (destructor :)


flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    zoom graph runTC
    updateNodes loc
    zoom graph runInterpreter
    updateValues loc
