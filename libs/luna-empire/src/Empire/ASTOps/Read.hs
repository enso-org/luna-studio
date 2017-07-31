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
import           LunaStudio.Data.Port               as Port
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

cutThroughGroups :: GraphOp m => NodeRef -> m NodeRef
cutThroughGroups r = match r $ \case
    Grouped g -> cutThroughGroups =<< IR.source g
    _         -> return r

getASTOutForPort :: GraphOp m => NodeId -> OutPortId -> m NodeRef
getASTOutForPort nodeId port = do
    lambda <- use Graph.breadcrumbHierarchy
    if lambda ^. BH.portMapping . _1 == nodeId
      then getLambdaInputForPort port =<< getTargetFromMarked (lambda ^. BH.self)
      else getOutputForPort      port =<< getASTVar nodeId

getLambdaInputForPort :: GraphOp m => OutPortId -> NodeRef -> m NodeRef
getLambdaInputForPort []                    lam = throwM PortDoesNotExistException
getLambdaInputForPort (Projection 0 : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam i _ -> getOutputForPort rest =<< IR.source i
    _       -> throwM PortDoesNotExistException
getLambdaInputForPort (Projection i : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam _ o -> getLambdaInputForPort (Projection (i - 1) : rest) =<< IR.source o
    _       -> throwM PortDoesNotExistException

getOutputForPort :: GraphOp m => OutPortId -> NodeRef -> m NodeRef
getOutputForPort []                    ref = cutThroughGroups ref
getOutputForPort (Projection i : rest) ref = cutThroughGroups ref >>= flip match `id` \case
    Cons _ as -> case as ^? ix i of
        Just s  -> getOutputForPort rest =<< IR.source s
        Nothing -> throwM PortDoesNotExistException
    _ -> throwM PortDoesNotExistException

isGraphNode :: GraphOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: GraphOp m => NodeRef -> m (Maybe NodeId)
getNodeId node = do
    rootNodeId <- preview (_Just . PortRef.srcNodeLoc . NodeLoc.nodeId) <$> IR.getLayer @Marker node
    varNodeId  <- (getVarNode node >>= getNodeId) `catch` (\(_e :: NotUnifyException) -> return Nothing)
    return $ rootNodeId <|> varNodeId

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

getVarsInside :: GraphOp m => NodeRef -> m [NodeRef]
getVarsInside e = do
    isVar <- isJust <$> IRExpr.narrowTerm @IR.Var e
    if isVar then return [e] else concat <$> (mapM (getVarsInside <=< IR.source) =<< IR.inputs e)

rightMatchOperand :: GraphOp m => NodeRef -> m EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure b
    _         -> throwM $ NotUnifyException node

getTargetNode :: GraphOp m => NodeRef -> m NodeRef
getTargetNode node = rightMatchOperand node >>= IR.source

leftMatchOperand :: GraphOp m => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _ -> pure a
    _         -> throwM $ NotUnifyException node

getVarNode :: GraphOp m => NodeRef -> m NodeRef
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

getCurrentASTTarget' :: GraphOp m => m NodeRef
getCurrentASTTarget' = do
    ref <- use $ Graph.breadcrumbHierarchy . BH.self
    succs <- toList <$> IR.getLayer @IR.Succs ref
    if null succs then return ref else getTargetFromMarked ref

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
    Grouped g  -> IR.source g >>= getLambdaOutputRef
    Lam _ o    -> IR.source o >>= getLambdaOutputRef
    Seq _ r    -> IR.source r >>= getLambdaOutputRef
    Marked _ m -> IR.source m >>= getLambdaOutputRef
    _          -> return node

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

dumpPatternVars :: GraphOp m => NodeRef -> m [NodeRef]
dumpPatternVars ref = match ref $ \case
    Var _     -> return [ref]
    Cons _ as -> fmap concat $ mapM (dumpPatternVars <=< IR.source) as
    Grouped g -> dumpPatternVars =<< IR.source g
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
    isCons var

rhsIsLambda :: GraphOp m => NodeRef -> m Bool
rhsIsLambda ref = do
    rhs <- getTargetNode ref
    isLambda rhs

canEnterNode :: GraphOp m => NodeRef -> m Bool
canEnterNode ref = do
    match' <- isMatch ref
    if match' then rhsIsLambda ref else return False

classFunctions :: ClassOp m => NodeRef -> m [NodeRef]
classFunctions unit = IR.matchExpr unit $ \case
    IR.Unit _ _ klass -> do
        klass' <- IR.source klass
        IR.matchExpr klass' $ \case
            IR.ClsASG _ _ _ funs -> do
                funs' <- mapM IR.source funs
                catMaybes <$> forM funs' (\f -> IR.matchExpr f $ \case
                    IR.ASGRootedFunction{} -> return (Just f)
                    _                      -> return Nothing)

getMetadataRef :: ClassOp m => NodeRef -> m (Maybe NodeRef)
getMetadataRef unit = IR.matchExpr unit $ \case
    IR.Unit _ _ klass -> do
        klass' <- IR.source klass
        IR.matchExpr klass' $ \case
            IR.ClsASG _ _ _ funs -> do
                funs' <- mapM IR.source funs
                (Safe.headMay . catMaybes) <$> forM funs' (\f -> IR.matchExpr f $ \case
                    IR.Metadata{} -> return (Just f)
                    _             -> return Nothing)

getFunByName :: ClassOp m => String -> m NodeRef
getFunByName name = do
    cls <- use Graph.clsClass
    maybeFuns <- do
        funs <- classFunctions cls
        forM funs $ \fun -> IR.matchExpr fun $ \case
            IR.ASGRootedFunction n' _ -> do
                n <- getVarName' =<< IR.source n'
                return $ if nameToString n == name then Just fun else Nothing
    case catMaybes maybeFuns of
        [f] -> return f
