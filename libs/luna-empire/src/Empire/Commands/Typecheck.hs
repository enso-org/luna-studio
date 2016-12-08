{-# LANGUAGE OverloadedStrings #-}

module Empire.Commands.Typecheck where

import           Prologue                hiding (TypeRep)
import           Control.Monad.State     hiding (when)
import           Unsafe.Coerce           (unsafeCoerce)
import           Control.Monad.Except    (throwError)
import           Control.Monad           (forM_)
import           Control.Monad.Reader    (ask)
import           Data.List               (sort)
import qualified Data.Map                as Map
import           Data.Maybe              (isNothing, maybeToList)

import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import qualified Empire.Data.Graph                 as Graph
import           Empire.Data.Graph                 (Graph)
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import           Empire.API.Data.Node              (NodeId, nodeId)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult
import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.Empire
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import qualified Empire.Commands.GraphBuilder      as GraphBuilder
import qualified Empire.Commands.Publisher         as Publisher

import qualified StdLibMock                                      as StdLib
import qualified Luna.IR.Library.Symbol                          as Symbol
import qualified Old.Luna.Compilation.Stage.TypeCheck            as TypeCheck
import qualified Old.Luna.Compilation.Stage.TypeCheck.Class      as TypeCheckState
import           Old.Luna.Compilation.Stage.TypeCheck            (Loop (..), Sequence (..))
import           Luna.Pass.Inference.Literals                    (LiteralsPass (..))
import           Luna.Pass.Inference.Struct                      (StructuralInferencePass (..))
import           Luna.Pass.Inference.Unification                 (StrictUnificationPass (..))
import           Luna.Pass.Inference.Importing                   (SymbolImportingPass (..))
import           Luna.Pass.Inference.Scan                        (ScanPass (..))
import           Old.Luna.Syntax.Model.Network.Builder           (Sign (..))

import qualified Luna.Pass.Evaluation.Interpreter.Interpreter    as Interpreter

import qualified Empire.ASTOp                                    as ASTOp


getNodeValueReprs :: NodeId -> Command Graph (Either String AST.ValueRep)
getNodeValueReprs nid = do
    nodeRef <- GraphUtils.getASTPointer nid
    metaMay <- zoom Graph.ast $ AST.readMeta nodeRef
    case metaMay of
        Just meta -> if meta ^. NodeMeta.displayResult
            then do
                valRef <- GraphUtils.getASTVar nid
                zoom Graph.ast $ AST.getNodeValue valRef
            else   return $ Right $ AST.PlainVal ("", [])
        Nothing -> return $ Right $ AST.PlainVal ("", [])

collect pass = return ()
    {-putStrLn $ "After pass: " <> pass-}
    {-st <- TypeCheckState.get-}
    {-putStrLn $ "State is: " <> show st-}

runTC :: Command Graph ()
runTC = do
    allNodeIds <- uses Graph.nodeMapping $ Map.keys
    roots <- mapM GraphUtils.getASTPointer allNodeIds
    ast   <- use Graph.ast
    (_, g) <- TypeCheck.runT $ flip ASTOp.runGraph ast $ do
        Symbol.loadFunctions StdLib.symbols
        TypeCheckState.modify_ $ TypeCheckState.freshRoots .~ roots
        let seq3 a b c     = Sequence a $ Sequence b c
            seq4 a b c d   = Sequence a $ seq3 b c d
            seq5 a b c d e = Sequence a $ seq4 b c d e
        let tc = seq4
                     ScanPass
                     LiteralsPass
                     StructuralInferencePass
                     (Loop $ Sequence
                         (Loop $ Sequence
                             SymbolImportingPass
                             (Loop $ StrictUnificationPass Positive False))
                         (StrictUnificationPass Negative True))
        TypeCheck.runTCWithArtifacts tc collect
    Graph.ast .= g
    return ()

runInterpreter :: Command Graph ()
runInterpreter = do
    ast        <- use Graph.ast
    allNodes   <- uses Graph.breadcrumbHierarchy topLevelIDs
    refs       <- mapM GraphUtils.getASTPointer allNodes
    metas      <- zoom Graph.ast $ mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    evals      <- mapM GraphUtils.getASTVar sorted
    newAst     <- liftIO $ fmap snd $ flip ASTOp.runBuilder ast $ Interpreter.run evals
    Graph.ast .= newAst
    return ()

reportError :: GraphLocation -> NodeId -> Maybe (APIError.Error TypeRep) -> Command InterpreterEnv ()
reportError loc id err = do
    cachedErr <- uses errorsCache $ Map.lookup id
    when (cachedErr /= err) $ do
        errorsCache %= Map.alter (const err) id
        valuesCache %= Map.delete id
        case err of
            Just e  -> Publisher.notifyResultUpdate loc id (NodeResult.Error e)     0
            Nothing -> Publisher.notifyResultUpdate loc id (NodeResult.Value "" []) 0

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc = do
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    forM_ allNodeIds $ \id -> do
        ref <- zoom graph $ GraphUtils.getASTTarget id

        err <- zoom (graph . Graph.ast) $ AST.getError ref
        reportError loc id err

        rep <- zoom graph $ GraphBuilder.buildNode id
        cached <- uses nodesCache $ Map.lookup id
        when (cached /= Just rep) $ do
            Publisher.notifyNodeUpdate loc rep
            nodesCache %= Map.insert id rep
    edgeNodes  <- zoom graph $ GraphBuilder.buildEdgeNodes
    forM_ edgeNodes $ \edges -> forM_ edges $ \rep -> do
        let id = rep ^. nodeId
        cached <- uses nodesCache $ Map.lookup id
        when (cached /= Just rep) $ do
            Publisher.notifyNodeUpdate loc rep
            nodesCache %= Map.insert id rep

updateValues :: GraphLocation -> Command InterpreterEnv ()
updateValues loc = do
    dests <- use destructors
    liftIO $ sequence_ dests
    destructors .= []
    allNodeIds <- uses (graph . Graph.breadcrumbHierarchy) topLevelIDs
    forM_ allNodeIds $ \id -> do
        noErrors <- isNothing <$> uses errorsCache (Map.lookup id)
        when noErrors $ do
            val <- zoom graph $ getNodeValueReprs id
            case val of
                Left err -> reportError loc id $ Just $ APIError.RuntimeError err
                Right v  -> case v of
                        AST.PlainVal (name, val) -> do
                            cached <- uses valuesCache $ Map.lookup id
                            when (cached /= Just val) $ do
                                Publisher.notifyResultUpdate loc id (NodeResult.Value name val) 100
                                valuesCache %= Map.insert id val
                        AST.Listener lst -> do
                            commEnv <- ask
                            destructor <- liftIO $ lst $ \(name, val) -> void $ runEmpire commEnv () $ Publisher.notifyResultUpdate loc id (NodeResult.Value name val) 100
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
