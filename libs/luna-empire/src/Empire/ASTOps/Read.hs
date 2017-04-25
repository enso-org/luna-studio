{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|

This module consists only of operation that get information from
AST, without modifying it. They can still throw exceptions though.

-}

module Empire.ASTOps.Read where

import           Control.Monad                      ((>=>), (<=<), forM)
import           Control.Monad.Catch                (Handler(..), catches)
import           Data.Maybe                         (isJust)
import           Empire.Prelude
import           Prologue                           (preview)

import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.PortRef            as PortRef
import           Empire.API.Data.Port               as Port
import qualified Empire.API.Data.NodeLoc            as NodeLoc
import           Empire.ASTOp                       (ASTOp, match)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     NotLambdaException(..), PortDoesNotExistException (..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import           Empire.Data.Layers                 (Marker)

import qualified OCI.IR.Combinators as IRExpr
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR

cutThroughGroups :: ASTOp m => NodeRef -> m NodeRef
cutThroughGroups r = match r $ \case
    Grouped g -> cutThroughGroups =<< IR.source g
    _         -> return r

getASTOutForPort :: ASTOp m => NodeId -> OutPortId -> m NodeRef
getASTOutForPort nodeId port = do
    asLambda <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent
    case asLambda of
        Just l -> do
            if l ^. BH.portMapping . _1 == nodeId
              then getLambdaInputForPort port =<< getTargetFromNodeIdTarget (l ^. BH.self) 
              else getOutputForPort      port =<< getASTVar nodeId
        _ -> getOutputForPort port =<< getASTVar nodeId

getLambdaInputForPort :: ASTOp m => OutPortId -> NodeRef -> m NodeRef
getLambdaInputForPort []                    lam = throwM PortDoesNotExistException
getLambdaInputForPort (Projection 0 : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam i _ -> getOutputForPort rest =<< IR.source i
    _       -> throwM PortDoesNotExistException
getLambdaInputForPort (Projection i : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam _ o -> getLambdaInputForPort (Projection (i - 1) : rest) =<< IR.source o
    _       -> throwM PortDoesNotExistException

getOutputForPort :: ASTOp m => OutPortId -> NodeRef -> m NodeRef
getOutputForPort []                    ref = cutThroughGroups ref
getOutputForPort (Projection i : rest) ref = cutThroughGroups ref >>= flip match `id` \case
    Cons _ as -> case as ^? ix i of
        Just s  -> getOutputForPort rest =<< IR.source s
        Nothing -> throwM PortDoesNotExistException
    _ -> throwM PortDoesNotExistException

isGraphNode :: ASTOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
getNodeId node = preview (_Just . PortRef.srcNodeLoc . NodeLoc.nodeId) <$> IR.getLayer @Marker node

getPatternNames :: ASTOp m => NodeRef -> m [String]
getPatternNames node = match node $ \case
    Var n     -> return [nameToString n]
    Cons _ as -> do
        args  <- mapM IR.source as
        names <- mapM getPatternNames args
        return $ concat names
    Blank{}   -> return ["_"]

data NoNameException = NoNameException NodeRef
    deriving Show

instance Exception NoNameException where
    toException = astExceptionToException
    fromException = astExceptionFromException

getVarName :: ASTOp m => NodeRef -> m String
getVarName node = match node $ \case
    Var n    -> return $ nameToString n
    Cons n _ -> return $ pathNameToString n
    Blank{}  -> return "_"
    _        -> throwM $ NoNameException node

getVarsInside :: ASTOp m => NodeRef -> m [NodeRef]
getVarsInside e = do
    isVar <- isJust <$> IRExpr.narrowTerm @IR.Var e
    if isVar then return [e] else concat <$> (mapM (getVarsInside <=< IR.source) =<< IR.inputs e)

rightMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure b
    _         -> throwM $ NotUnifyException node

getTargetNode :: ASTOp m => NodeRef -> m NodeRef
getTargetNode node = rightMatchOperand node >>= IR.source

leftMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _ -> pure a
    _         -> throwM $ NotUnifyException node

getVarNode :: ASTOp m => NodeRef -> m NodeRef
getVarNode node = leftMatchOperand node >>= IR.source

safeGetVarNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
safeGetVarNodeId node = (getVarNode node >>= getNodeId) `catch`
    (\(_e :: NotUnifyException) -> return Nothing)

data NodeDoesNotExistException = NodeDoesNotExistException NodeId
    deriving Show
instance Exception NodeDoesNotExistException where
    toException = astExceptionToException
    fromException = astExceptionFromException

getASTRef :: ASTOp m => NodeId -> m BH.NodeIDTarget
getASTRef nodeId = preuse (Graph.breadcrumbHierarchy . BH.children . ix nodeId . BH.self) <?!> NodeDoesNotExistException nodeId

getASTPointer :: ASTOp m => NodeId -> m NodeRef
getASTPointer nodeId = view BH.anyRef <$> getASTRef nodeId

getCurrentASTPointer :: ASTOp m => m (Maybe NodeRef)
getCurrentASTPointer = preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.self . BH.anyRef

getTargetFromNodeIdTarget :: ASTOp m => BH.NodeIDTarget -> m NodeRef
getTargetFromNodeIdTarget (BH.AnonymousNode r) = return r
getTargetFromNodeIdTarget (BH.MatchNode     u) = getTargetNode u

getASTMarkerPosition :: ASTOp m => NodeId -> m NodeRef
getASTMarkerPosition nodeId = do
    ref <- getASTRef nodeId
    case ref of
        BH.AnonymousNode n -> return n
        BH.MatchNode     n -> getVarNode n

getASTTarget :: ASTOp m => NodeId -> m NodeRef
getASTTarget nodeId = do
    ref <- getASTRef nodeId
    getTargetFromNodeIdTarget ref

getCurrentASTTarget :: ASTOp m => m (Maybe NodeRef)
getCurrentASTTarget = do
    ref <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.self
    mapM getTargetFromNodeIdTarget ref

getASTVar :: ASTOp m => NodeId -> m NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    getVarNode matchNode

getCurrentASTVar :: ASTOp m => m (Maybe NodeRef)
getCurrentASTVar = mapM getVarNode =<< getCurrentASTPointer

getSelfNodeRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getSelfNodeRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc node = match node $ \case
    Acc t _ -> IR.source t >>= getSelfNodeRef' True
    App t _ -> IR.source t >>= getSelfNodeRef' seenAcc
    _       -> return $ if seenAcc then Just node else Nothing

getLambdaBodyRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getLambdaBodyRef lam = do
    seqRef <- getLambdaSeqRef lam
    forM seqRef $ flip match $ \case
        Seq l _r -> IR.source l

getLambdaSeqRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef = getLambdaSeqRef' False

getLambdaSeqRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef' firstLam node = match node $ \case
    Grouped g  -> IR.source g >>= getLambdaSeqRef' firstLam
    Lam _ next -> do
        nextLam <- IR.source next
        getLambdaSeqRef' True nextLam
    Seq{}     -> if firstLam then return $ Just node else throwM $ NotLambdaException node
    _         -> if firstLam then return Nothing     else throwM $ NotLambdaException node

getLambdaOutputRef :: ASTOp m => NodeRef -> m NodeRef
getLambdaOutputRef = getLambdaOutputLink >=> IR.source

getLambdaOutputLink :: ASTOp m => NodeRef -> m EdgeRef
getLambdaOutputLink = getLambdaOutputLink' False

getLambdaOutputLink' :: ASTOp m => Bool -> NodeRef -> m EdgeRef
getLambdaOutputLink' firstLam node = match node $ \case
    Grouped g  -> IR.source g >>= getLambdaOutputLink' firstLam
    Lam _ next -> do
        nextLam <- IR.source next
        match nextLam $ \case
            Lam{} -> getLambdaOutputLink' True nextLam
            Seq{} -> getLambdaOutputLink' True nextLam
            _     -> return next
    Seq _l r  -> if firstLam then return r else throwM $ NotLambdaException node

getFirstNonLambdaRef :: ASTOp m => NodeRef -> m NodeRef
getFirstNonLambdaRef = getFirstNonLambdaLink >=> IR.source

getFirstNonLambdaLink :: ASTOp m => NodeRef -> m EdgeRef
getFirstNonLambdaLink node = match node $ \case
    Grouped g  -> IR.source g >>= getFirstNonLambdaLink
    Lam _ next -> do
        nextLam <- IR.source next
        match nextLam $ \case
            Lam{} -> getFirstNonLambdaLink nextLam
            _     -> return next
    _         -> throwM $ NotLambdaException node

isApp :: ASTOp m => NodeRef -> m Bool
isApp expr = isJust <$> IRExpr.narrowTerm @IR.App expr

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank expr = isJust <$> IRExpr.narrowTerm @IR.Blank expr

isLambda :: ASTOp m => NodeRef -> m Bool
isLambda expr = match expr $ \case
    Lam{}     -> return True
    Grouped g -> IR.source g >>= isLambda
    _         -> return False

isMatch :: ASTOp m => NodeRef -> m Bool
isMatch expr = isJust <$> IRExpr.narrowTerm @IR.Unify expr

isCons :: ASTOp m => NodeRef -> m Bool
isCons expr = isJust <$> IRExpr.narrowTerm @IR.Cons expr

dumpPatternVars :: ASTOp m => NodeRef -> m [NodeRef]
dumpPatternVars ref = match ref $ \case
    Var _     -> return [ref]
    Cons _ as -> fmap concat $ mapM (dumpPatternVars <=< IR.source) as
    Grouped g -> dumpPatternVars =<< IR.source g
    _         -> return []

nodeIsPatternMatch :: ASTOp m => NodeId -> m Bool
nodeIsPatternMatch nid = (do
    root <- getASTPointer nid
    varIsPatternMatch root) `catches` [
          Handler (\(e :: NotUnifyException)         -> return False)
        , Handler (\(e :: NodeDoesNotExistException) -> return False)
        ]

varIsPatternMatch :: ASTOp m => NodeRef -> m Bool
varIsPatternMatch expr = do
    var <- getVarNode expr
    isCons var

rhsIsLambda :: ASTOp m => NodeRef -> m Bool
rhsIsLambda ref = do
    rhs <- getTargetNode ref
    isLambda rhs

canEnterNode :: ASTOp m => NodeRef -> m Bool
canEnterNode ref = do
    match' <- isMatch ref
    if match' then rhsIsLambda ref else return False
