{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import qualified Safe

import           LunaStudio.Data.Node               (NodeId)
import qualified LunaStudio.Data.PortRef            as PortRef
import           LunaStudio.Data.Port               (OutPortId(..), OutPortIndex(..))
import qualified LunaStudio.Data.NodeLoc            as NodeLoc
import           Empire.ASTOp                       (ClassOp, GraphOp, ASTOp, match)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     NotLambdaException(..), PortDoesNotExistException (..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import           Empire.Data.Layers                 (Marker)

import qualified OCI.IR.Combinators as IRExpr
import           Luna.IR.Term.Uni
import qualified Luna.IR as IR

import qualified System.IO as IO

cutThroughGroups :: GraphOp m => NodeRef -> m NodeRef
cutThroughGroups r = match r $ \case
    Grouped g -> cutThroughGroups =<< IR.source g
    _         -> return r

cutThroughMarked :: ClassOp m => NodeRef -> m NodeRef
cutThroughMarked r = match r $ \case
    Marked m expr -> cutThroughMarked =<< IR.source expr
    _             -> return r

cutThroughDoc :: ClassOp m => NodeRef -> m NodeRef
cutThroughDoc r = match r $ \case
    Documented _d expr -> cutThroughDoc =<< IR.source expr
    _                  -> return r

cutThroughDocAndMarked :: ClassOp m => NodeRef -> m NodeRef
cutThroughDocAndMarked r = match r $ \case
    Marked _m expr  -> cutThroughDocAndMarked =<< IR.source expr
    Documented _d a -> cutThroughDocAndMarked =<< IR.source a
    _               -> return r

isInputSidebar :: GraphOp m => NodeId -> m Bool
isInputSidebar nid = do
    lambda <- use Graph.breadcrumbHierarchy
    return $ lambda ^. BH.portMapping . _1 == nid

getASTOutForPort :: GraphOp m => NodeId -> OutPortId -> m NodeRef
getASTOutForPort nodeId port = do
    isSidebar <- isInputSidebar nodeId
    if isSidebar
      then getLambdaInputForPort port =<< getTargetFromMarked =<< use (Graph.breadcrumbHierarchy . BH.self)
      else getOutputForPort      port =<< getASTVar nodeId

getLambdaInputForPort :: GraphOp m => OutPortId -> NodeRef -> m NodeRef
getLambdaInputForPort []                           lam = throwM $ PortDoesNotExistException []
getLambdaInputForPort portId@(Projection 0 : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam i _            -> getOutputForPort rest =<< IR.source i
    ASGFunction _ as _ -> case as of
        (a : _) -> IR.source a
        _       -> throwM $ PortDoesNotExistException portId
    _                  -> throwM $ PortDoesNotExistException portId
getLambdaInputForPort portId@(Projection i : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam _ o            -> getLambdaInputForPort (Projection (i - 1) : rest) =<< IR.source o
    ASGFunction _ as _ -> case as ^? ix i of
        Just a -> IR.source a
        _      -> throwM $ PortDoesNotExistException portId
    _                  -> throwM $ PortDoesNotExistException portId

getOutputForPort :: GraphOp m => OutPortId -> NodeRef -> m NodeRef
getOutputForPort []                           ref = cutThroughGroups ref
getOutputForPort portId@(Projection i : rest) ref = cutThroughGroups ref >>= flip match `id` \case
    Cons _ as     | Just s <- as ^? ix i   -> getOutputForPort rest =<< IR.source s
    IR.List args  | Just s <- args ^? ix i -> getOutputForPort rest =<< IR.source s
    IR.Tuple args | Just s <- args ^? ix i -> getOutputForPort rest =<< IR.source s
    _ -> throwM $ PortDoesNotExistException portId

isGraphNode :: GraphOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp g m => NodeRef -> m (Maybe NodeId)
getNodeId node = do
    rootNodeId <- preview (_Just . PortRef.srcNodeLoc . NodeLoc.nodeId) <$> IR.getLayer @Marker node
    varNodeId  <- (getVarNode node >>= getNodeId) `catch` (\(_e :: NotUnifyException) -> return Nothing)
    varsInside <- (getVarsInside =<< getVarNode node) `catch` (\(_e :: NotUnifyException) -> return [])
    varsNodeIds <- mapM getNodeId varsInside
    let leavesNodeId = foldl' (<|>) Nothing varsNodeIds
    return $ rootNodeId <|> varNodeId <|> leavesNodeId

getPatternNames :: GraphOp m => NodeRef -> m [String]
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

getVarName' :: ASTOp a m => NodeRef -> m IR.Name
getVarName' node = match node $ \case
    Var n    -> return n
    Cons n _ -> return n
    Blank{}  -> return "_"
    _        -> throwM $ NoNameException node

getVarName :: ASTOp a m => NodeRef -> m String
getVarName = fmap nameToString . getVarName'

getVarsInside :: ASTOp g m => NodeRef -> m [NodeRef]
getVarsInside e = do
    isVar <- isJust <$> IRExpr.narrowTerm @IR.Var e
    if isVar then return [e] else concat <$> (mapM (getVarsInside <=< IR.source) =<< IR.inputs e)

rightMatchOperand :: GraphOp m => NodeRef -> m EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure b
    _         -> throwM $ NotUnifyException node

getTargetNode :: GraphOp m => NodeRef -> m NodeRef
getTargetNode node = rightMatchOperand node >>= IR.source

leftMatchOperand :: ASTOp g m => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _         -> pure a
    ASGFunction n _ _ -> pure n
    _         -> throwM $ NotUnifyException node

getVarNode :: ASTOp g m => NodeRef -> m NodeRef
getVarNode node = leftMatchOperand node >>= IR.source

data NodeDoesNotExistException = NodeDoesNotExistException NodeId
    deriving Show
instance Exception NodeDoesNotExistException where
    toException = astExceptionToException
    fromException = astExceptionFromException

data MalformedASTRef = MalformedASTRef NodeRef
    deriving Show
instance Exception MalformedASTRef where
    toException = astExceptionToException
    fromException = astExceptionFromException



getASTRef :: GraphOp m => NodeId -> m NodeRef
getASTRef nodeId = preuse (Graph.breadcrumbHierarchy . BH.children . ix nodeId . BH.self) <?!> NodeDoesNotExistException nodeId

getASTPointer :: GraphOp m => NodeId -> m NodeRef
getASTPointer nodeId = do
    marked <- getASTRef nodeId
    match marked $ \case
        IR.Marked _m expr -> IR.source expr
        _                 -> return marked

getCurrentASTPointer :: GraphOp m => m NodeRef
getCurrentASTPointer = do
    ref <- getCurrentASTRef
    IR.matchExpr ref $ \case
        IR.Marked _ expr -> IR.source expr
        _                -> return ref

getCurrentASTRef :: GraphOp m => m NodeRef
getCurrentASTRef = use $ Graph.breadcrumbHierarchy . BH.self

-- TODO[MK]: Fail when not marked and unify with getTargetEdge
getTargetFromMarked :: GraphOp m => NodeRef -> m NodeRef
getTargetFromMarked marked = match marked $ \case
    IR.Marked _m expr -> do
        expr' <- IR.source expr
        match expr' $ \case
            IR.Unify l r -> IR.source r
            _            -> return expr'
    _ -> return marked


getVarEdge :: GraphOp m => NodeId -> m EdgeRef
getVarEdge nid = do
    ref <- getASTRef nid
    match ref $ \case
        IR.Marked _m expr -> do
            expr' <- IR.source expr
            match expr' $ \case
                IR.Unify l r -> return l
                _            -> throwM $ NotUnifyException expr'
        _ -> throwM $ MalformedASTRef ref

getTargetEdge :: GraphOp m => NodeId -> m EdgeRef
getTargetEdge nid = do
    ref <- getASTRef nid
    match ref $ \case
        IR.Marked _m expr -> do
            expr' <- IR.source expr
            match expr' $ \case
                IR.Unify l r -> return r
                _            -> return expr
        _ -> throwM $ MalformedASTRef ref

getNameOf :: GraphOp m => NodeRef -> m (Maybe Text)
getNameOf ref = match ref $ \case
    IR.Marked _ e -> getNameOf =<< IR.source e
    IR.Unify  l _ -> getNameOf =<< IR.source l
    IR.Var    n   -> return $ Just $ convert n
    _             -> return Nothing

getASTMarkerPosition :: GraphOp m => NodeId -> m NodeRef
getASTMarkerPosition nodeId = do
    ref <- getASTPointer nodeId
    match ref $ \case
        IR.Unify l r -> IR.source l
        _            -> return ref

getMarkerNode :: GraphOp m => NodeRef -> m (Maybe NodeRef)
getMarkerNode ref = match ref $ \case
    IR.Marked m _expr -> Just <$> IR.source m
    _                 -> return Nothing

getASTTarget :: GraphOp m => NodeId -> m NodeRef
getASTTarget nodeId = do
    ref   <- getASTRef nodeId
    getTargetFromMarked ref

getCurrentASTTarget :: GraphOp m => m NodeRef
getCurrentASTTarget = do
    ref <- use $ Graph.breadcrumbHierarchy . BH.self
    getTargetFromMarked ref

getCurrentBody :: GraphOp m => m NodeRef
getCurrentBody = getFirstNonLambdaRef =<< getCurrentASTTarget

getASTVar :: GraphOp m => NodeId -> m NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    getVarNode matchNode

getCurrentASTVar :: GraphOp m => m NodeRef
getCurrentASTVar = getVarNode =<< getCurrentASTPointer

getSelfNodeRef :: GraphOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getSelfNodeRef' :: GraphOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc node = match node $ \case
    Acc t _ -> IR.source t >>= getSelfNodeRef' True
    App t _ -> IR.source t >>= getSelfNodeRef' seenAcc
    _       -> return $ if seenAcc then Just node else Nothing

getLambdaBodyRef :: GraphOp m => NodeRef -> m (Maybe NodeRef)
getLambdaBodyRef lam = match lam $ \case
    Lam _ o -> getLambdaBodyRef =<< IR.source o
    _       -> return $ Just lam

getLambdaSeqRef :: GraphOp m => NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef = getLambdaSeqRef' False

getLambdaSeqRef' :: GraphOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getLambdaSeqRef' firstLam node = match node $ \case
    Grouped g  -> IR.source g >>= getLambdaSeqRef' firstLam
    Lam _ next -> do
        nextLam <- IR.source next
        getLambdaSeqRef' True nextLam
    Seq{}     -> if firstLam then return $ Just node else throwM $ NotLambdaException node
    _         -> if firstLam then return Nothing     else throwM $ NotLambdaException node

getLambdaOutputRef :: GraphOp m => NodeRef -> m NodeRef
getLambdaOutputRef node = match node $ \case
    ASGFunction _ _ b -> IR.source b >>= getLambdaOutputRef
    Grouped g         -> IR.source g >>= getLambdaOutputRef
    Lam _ o           -> IR.source o >>= getLambdaOutputRef
    Seq _ r           -> IR.source r >>= getLambdaOutputRef
    Marked _ m        -> IR.source m >>= getLambdaOutputRef
    _                 -> return node

getFirstNonLambdaRef :: GraphOp m => NodeRef -> m NodeRef
getFirstNonLambdaRef ref = do
    link <- getFirstNonLambdaLink ref
    maybe (return ref) (IR.source) link

getFirstNonLambdaLink :: GraphOp m => NodeRef -> m (Maybe EdgeRef)
getFirstNonLambdaLink node = match node $ \case
    ASGFunction _ _ o -> return $ Just o
    Grouped g         -> IR.source g >>= getFirstNonLambdaLink
    Lam _ next        -> do
        nextLam <- IR.source next
        match nextLam $ \case
            Lam{} -> getFirstNonLambdaLink nextLam
            _     -> return $ Just next
    _         -> return Nothing

isApp :: GraphOp m => NodeRef -> m Bool
isApp expr = isJust <$> IRExpr.narrowTerm @IR.App expr

isBlank :: GraphOp m => NodeRef -> m Bool
isBlank expr = isJust <$> IRExpr.narrowTerm @IR.Blank expr

isLambda :: GraphOp m => NodeRef -> m Bool
isLambda expr = match expr $ \case
    Lam{}     -> return True
    Grouped g -> IR.source g >>= isLambda
    _         -> return False

isEnterable :: GraphOp m => NodeRef -> m Bool
isEnterable expr = match expr $ \case
    Lam{}         -> return True
    ASGFunction{} -> return True
    Grouped g     -> IR.source g >>= isEnterable
    _             -> return False

isMatch :: GraphOp m => NodeRef -> m Bool
isMatch expr = isJust <$> IRExpr.narrowTerm @IR.Unify expr

isCons :: GraphOp m => NodeRef -> m Bool
isCons expr = isJust <$> IRExpr.narrowTerm @IR.Cons expr

isVar :: GraphOp m => NodeRef -> m Bool
isVar expr = isJust <$> IRExpr.narrowTerm @IR.Var expr

isTuple :: GraphOp m => NodeRef -> m Bool
isTuple expr = isJust <$> IRExpr.narrowTerm @IR.Tuple expr

isASGFunction :: GraphOp m => NodeRef -> m Bool
isASGFunction expr = isJust <$> IRExpr.narrowTerm @IR.ASGFunction expr

isAnonymous :: GraphOp m => NodeRef -> m Bool
isAnonymous expr = match expr $ \case
    Marked _ e -> isAnonymous =<< IR.source e
    Unify _ _  -> return False
    _          -> return True

dumpPatternVars :: GraphOp m => NodeRef -> m [NodeRef]
dumpPatternVars ref = match ref $ \case
    Var _     -> return [ref]
    Cons _ as -> fmap concat $ mapM (dumpPatternVars <=< IR.source) as
    Grouped g -> dumpPatternVars =<< IR.source g
    Tuple a   -> fmap concat $ mapM (dumpPatternVars <=< IR.source) a
    _         -> return []

nodeIsPatternMatch :: GraphOp m => NodeId -> m Bool
nodeIsPatternMatch nid = (do
    root <- getASTPointer nid
    varIsPatternMatch root) `catches` [
          Handler (\(e :: NotUnifyException)         -> return False)
        , Handler (\(e :: NodeDoesNotExistException) -> return False)
        ]

varIsPatternMatch :: GraphOp m => NodeRef -> m Bool
varIsPatternMatch expr = do
    var <- getVarNode expr
    not <$> isVar var

rhsIsLambda :: GraphOp m => NodeRef -> m Bool
rhsIsLambda ref = do
    rhs <- getTargetNode ref
    isLambda rhs

canEnterNode :: GraphOp m => NodeRef -> m Bool
canEnterNode ref = do
    match' <- isMatch ref
    if match' then rhsIsLambda ref else return False

classFunctions :: ClassOp m => NodeRef -> m [NodeRef]
classFunctions unit = do
    klass' <- classFromUnit unit
    IR.matchExpr klass' $ \case
        IR.ClsASG _ _ _ _ funs -> do
            funs' <- mapM IR.source funs
            catMaybes <$> forM funs' (\f -> cutThroughDocAndMarked f >>= \fun -> IR.matchExpr fun $ \case
                IR.ASGRootedFunction{} -> return (Just f)
                _                      -> return Nothing)
        _ -> return []

classFromUnit :: ClassOp m => NodeRef -> m NodeRef
classFromUnit unit = IR.matchExpr unit $ \case
    IR.Unit _ _ c -> IR.source c

getMetadataRef :: ClassOp m => NodeRef -> m (Maybe NodeRef)
getMetadataRef unit = do
    klass' <- classFromUnit unit
    IR.matchExpr klass' $ \case
        IR.ClsASG _ _ _ _ funs -> do
            funs' <- mapM IR.source funs
            (Safe.headMay . catMaybes) <$> forM funs' (\f -> IR.matchExpr f $ \case
                IR.Metadata{} -> return (Just f)
                _             -> return Nothing)
        _ -> return Nothing

getFunByNodeId :: ClassOp m => NodeId -> m NodeRef
getFunByNodeId nodeId = do
    cls  <- use Graph.clsClass
    funs <- classFunctions cls
    fs   <- forM funs $ \fun -> do
        nid <- getNodeId fun
        return $ if nid == Just nodeId then Just fun else Nothing
    case catMaybes fs of
        []  -> throwM $ NodeDoesNotExistException nodeId
        [f] -> return f
        _   -> error $ "multiple functions with " <> show nodeId
