{-# LANGUAGE PartialTypeSignatures #-}
{-|

This module consists only of operation that get information from
AST, without modifying it. They can still throw exceptions though.

-}

module Empire.ASTOps.Read where

import           Control.Monad.Catch                (Handler(..), catches)
import           Data.Maybe                         (isJust)
import           Empire.Prelude
import           Control.Lens                       (preview)
import qualified Safe

import           LunaStudio.Data.NodeId             (NodeId)
import           Data.Graph.Component.Node.Class    (Nodes)
import qualified Luna.Pass.Data.Layer.PortMarker    as PortMarker
import           Data.Graph.Data.Component.Class    (Component)
import qualified Data.Graph.Data.Layer.Class        as Layer

import           LunaStudio.Data.Port               (OutPortId, OutPortIndex(..))
import           Empire.ASTOp                       (ClassOp, GraphOp, ASTOp, match)
import           Empire.Data.AST                    (NodeRef, EdgeRef, NotUnifyException(..),
                                                     NotLambdaException(..), PortDoesNotExistException (..),
                                                     astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH
import           Empire.Data.Layers                 (Marker)

import qualified Luna.IR as IR
import qualified Luna.IR.Term.Ast.Invalid as IR


cutThroughGroups :: NodeRef -> GraphOp NodeRef
cutThroughGroups r = match r $ \case
    Grouped g -> cutThroughGroups =<< source g
    _         -> return r

cutThroughMarked :: NodeRef -> ClassOp NodeRef
cutThroughMarked r = match r $ \case
    Marked _ expr -> cutThroughMarked =<< source expr
    _             -> return r

cutThroughDoc :: NodeRef -> ClassOp NodeRef
cutThroughDoc r = match r $ \case
    Documented _d expr -> cutThroughDoc =<< source expr
    _                  -> return r

cutThroughDocAndMarked :: NodeRef -> ASTOp g NodeRef
cutThroughDocAndMarked r = match r $ \case
    Marked _m expr  -> cutThroughDocAndMarked =<< source expr
    Documented _d a -> cutThroughDocAndMarked =<< source a
    _               -> return r

isInputSidebar :: NodeId -> GraphOp Bool
isInputSidebar nid = do
    lambda <- use Graph.breadcrumbHierarchy
    return $ lambda ^. BH.portMapping . _1 == nid

getASTOutForPort :: NodeId -> OutPortId -> GraphOp NodeRef
getASTOutForPort nodeId port = do
    isSidebar <- isInputSidebar nodeId
    if isSidebar
      then getLambdaInputForPort port =<< getTargetFromMarked =<< use (Graph.breadcrumbHierarchy . BH.self)
      else getOutputForPort      port =<< getASTVar nodeId

getLambdaInputForPort :: OutPortId -> NodeRef -> GraphOp NodeRef
getLambdaInputForPort []                           _   = throwM $ PortDoesNotExistException []
getLambdaInputForPort portId@(Projection 0 : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam i _             -> getOutputForPort rest =<< source i
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        case as of
            (a : _) -> source a
            _       -> throwM $ PortDoesNotExistException portId
    _                  -> throwM $ PortDoesNotExistException portId
getLambdaInputForPort portId@(Projection i : rest) lam = cutThroughGroups lam >>= flip match `id` \case
    Lam _ o             -> getLambdaInputForPort (Projection (i - 1) : rest) =<< source o
    ASGFunction _ as' _ -> do
        as <- ptrListToList as'
        case as ^? ix i of
            Just a -> source a
            _      -> throwM $ PortDoesNotExistException portId
    _                  -> throwM $ PortDoesNotExistException portId

getOutputForPort :: OutPortId -> NodeRef -> GraphOp NodeRef
getOutputForPort []                           ref = cutThroughGroups ref
getOutputForPort portId@(Projection i : rest) ref = cutThroughGroups ref >>= flip match `id` \case
    Cons _ as' -> do
        as <- ptrListToList as'
        case as   ^? ix i of
            Just s -> getOutputForPort rest =<< source s
            _      -> throwM $ PortDoesNotExistException portId 
    List as'   -> do
        as <- ptrListToList as'
        case as ^? ix i of
            Just s -> getOutputForPort rest =<< source s
            _      -> throwM $ PortDoesNotExistException portId 
    Tuple as'  -> do
        as <- ptrListToList as'
        case as ^? ix i of
            Just s -> getOutputForPort rest =<< source s
            _      -> throwM $ PortDoesNotExistException portId 
    _ -> throwM $ PortDoesNotExistException portId

isGraphNode ::
    _ => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId ::
    ( MonadCatch m
    , Layer.Reader (Component Nodes) Marker m
    , Layer.Reader (Component Nodes) IR.Model m
    , Layer.Reader IR.Link IR.Source m
    ) => NodeRef -> m (Maybe NodeId)
getNodeId node = do
    rootNodeId <- preview (_Just . PortMarker.srcNodeLoc) <$> getLayer @Marker node
    varNodeId  <- (getVarNode node >>= getNodeId) `catch` (\(_e :: NotUnifyException) -> return Nothing)
    varsInside <- (getVarsInside =<< getVarNode node) `catch` (\(_e :: NotUnifyException) -> return [])
    varsNodeIds <- mapM getNodeId varsInside
    let leavesNodeId = foldl' (<|>) Nothing varsNodeIds
    return $ rootNodeId <|> varNodeId <|> leavesNodeId

getPatternNames :: NodeRef -> GraphOp [String]
getPatternNames node = match node $ \case
    Var n     -> return [nameToString n]
    Cons _ as' -> do
        as <- ptrListToList as'
        args  <- mapM source as
        names <- mapM getPatternNames args
        return $ concat names
    Blank{}   -> return ["_"]

data NoNameException = NoNameException NodeRef
    deriving Show

instance Exception NoNameException where
    toException = astExceptionToException
    fromException = astExceptionFromException

data InvalidNameException = InvalidNameException NodeRef
    deriving Show

instance Exception InvalidNameException where
    toException = astExceptionToException
    fromException = astExceptionFromException

getVarName' :: NodeRef -> ASTOp g IR.Name
getVarName' node = match node $ \case
    Var n    -> return n
    Cons n _ -> return n
    Blank{}  -> return "_"
    Invalid IR.InvalidFunctionName -> throwM $ InvalidNameException node
    Invalid IR.MissingFunctionName -> throwM $ InvalidNameException node
    _        -> throwM $ NoNameException node

getVarName :: NodeRef -> ASTOp g String
getVarName = fmap nameToString . getVarName'

getVarsInside ::
    ( Layer.Reader (Component Nodes) IR.Model m
    , Layer.Reader IR.Link IR.Source m
    ) => NodeRef -> m [NodeRef]
getVarsInside e = do
    var <- isVar e
    if var then return [e] else concat <$> (mapM (getVarsInside <=< source) =<< inputs e)

rightMatchOperand :: NodeRef -> GraphOp EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure $ generalize b
    _         -> throwM $ NotUnifyException node

getTargetNode :: NodeRef -> GraphOp NodeRef
getTargetNode node = rightMatchOperand node >>= source

leftMatchOperand ::
    ( Layer.Reader (Component Nodes) IR.Model m
    , MonadThrow m
    ) => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _         -> pure $ generalize a
    ASGFunction n _ _ -> pure $ generalize n
    _         -> throwM $ NotUnifyException node

getVarNode ::
    ( MonadThrow m
    , Layer.Reader (Component Nodes) IR.Model m
    , Layer.Reader IR.Link IR.Source m
    ) => NodeRef -> m NodeRef
getVarNode node = leftMatchOperand node >>= source

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



getASTRef :: NodeId -> GraphOp NodeRef
getASTRef nodeId = preuse (Graph.breadcrumbHierarchy . BH.children . ix nodeId . BH.self) <?!> NodeDoesNotExistException nodeId

getASTPointer :: NodeId -> GraphOp NodeRef
getASTPointer nodeId = do
    marked <- getASTRef nodeId
    match marked $ \case
        Marked _m expr -> source expr
        _              -> return marked

getCurrentASTPointer :: GraphOp NodeRef
getCurrentASTPointer = do
    ref <- getCurrentASTRef
    match ref $ \case
        Marked _ expr -> source expr
        _                -> return ref

getCurrentASTRef :: GraphOp NodeRef
getCurrentASTRef = use $ Graph.breadcrumbHierarchy . BH.self

-- TODO[MK]: Fail when not marked and unify with getTargetEdge
getTargetFromMarked :: NodeRef -> GraphOp NodeRef
getTargetFromMarked marked = match marked $ \case
    Marked _m expr -> do
        expr' <- source expr
        match expr' $ \case
            Unify _ r -> source r
            _         -> return expr'
    _ -> return marked


getVarEdge :: NodeId -> GraphOp EdgeRef
getVarEdge nid = do
    ref <- getASTRef nid
    match ref $ \case
        Marked _m expr -> do
            expr' <- source expr
            match expr' $ \case
                Unify l _ -> return $ generalize l
                _         -> throwM $ NotUnifyException expr'
        _ -> throwM $ MalformedASTRef ref

getTargetEdge :: NodeId -> GraphOp EdgeRef
getTargetEdge nid = do
    ref <- getASTRef nid
    match ref $ \case
        Marked _m expr -> do
            expr' <- source expr
            match expr' $ \case
                Unify _ r -> return $ generalize r
                _         -> return $ generalize expr
        _ -> throwM $ MalformedASTRef ref

getNameOf :: NodeRef -> GraphOp (Maybe Text)
getNameOf ref = match ref $ \case
    Marked _ e -> getNameOf =<< source e
    Unify  l _ -> getNameOf =<< source l
    Var    n   -> return $ Just $ convert $ convertTo @String n
    _             -> return Nothing

getASTMarkerPosition :: NodeId -> GraphOp NodeRef
getASTMarkerPosition nodeId = do
    ref <- getASTPointer nodeId
    match ref $ \case
        Unify l _ -> source l
        _         -> return ref

getMarkerNode :: NodeRef -> GraphOp (Maybe NodeRef)
getMarkerNode ref = match ref $ \case
    Marked m _expr -> Just <$> source m
    _                 -> return Nothing

getASTTarget :: NodeId -> GraphOp NodeRef
getASTTarget nodeId = do
    ref   <- getASTRef nodeId
    getTargetFromMarked ref

getCurrentASTTarget :: GraphOp NodeRef
getCurrentASTTarget = do
    ref <- use $ Graph.breadcrumbHierarchy . BH.self
    getTargetFromMarked ref

getCurrentBody :: GraphOp NodeRef
getCurrentBody = getFirstNonLambdaRef =<< getCurrentASTTarget

getASTVar :: NodeId -> GraphOp NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    getVarNode matchNode

getCurrentASTVar :: GraphOp NodeRef
getCurrentASTVar = getVarNode =<< getCurrentASTPointer

getSelfNodeRef :: NodeRef -> GraphOp (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getSelfNodeRef' :: Bool -> NodeRef -> GraphOp (Maybe NodeRef)
getSelfNodeRef' seenAcc node = match node $ \case
    Acc t _ -> source t >>= getSelfNodeRef' True
    App t _ -> source t >>= getSelfNodeRef' seenAcc
    _       -> return $ if seenAcc then Just node else Nothing

getLambdaSeqRef :: NodeRef -> GraphOp (Maybe NodeRef)
getLambdaSeqRef = getLambdaSeqRef' False

getLambdaSeqRef' :: Bool -> NodeRef -> GraphOp (Maybe NodeRef)
getLambdaSeqRef' firstLam node = match node $ \case
    Grouped g  -> source g >>= getLambdaSeqRef' firstLam
    Lam _ next -> do
        nextLam <- source next
        getLambdaSeqRef' True nextLam
    Seq{}     -> if firstLam then return $ Just node else throwM $ NotLambdaException node
    _         -> if firstLam then return Nothing     else throwM $ NotLambdaException node

getLambdaOutputRef :: NodeRef -> GraphOp NodeRef
getLambdaOutputRef node = match node $ \case
    ASGFunction _ _ b -> source b >>= getLambdaOutputRef
    Grouped g         -> source g >>= getLambdaOutputRef
    Lam _ o           -> source o >>= getLambdaOutputRef
    Seq _ r           -> source r >>= getLambdaOutputRef
    Marked _ m        -> source m >>= getLambdaOutputRef
    _                 -> return node

getFirstNonLambdaRef :: NodeRef -> GraphOp NodeRef
getFirstNonLambdaRef ref = do
    link' <- getFirstNonLambdaLink ref
    maybe (return ref) (source) link'

getFirstNonLambdaLink :: NodeRef -> GraphOp (Maybe EdgeRef)
getFirstNonLambdaLink node = match node $ \case
    ASGFunction _ _ o -> return $ Just $ generalize o
    Grouped g         -> source g >>= getFirstNonLambdaLink
    Lam _ next        -> do
        nextLam <- source next
        match nextLam $ \case
            Lam{} -> getFirstNonLambdaLink nextLam
            _     -> return $ Just $ generalize next
    _         -> return Nothing

isApp :: NodeRef -> GraphOp Bool
-- isApp expr = isJust <$> narrowTerm @IR.App expr
isApp expr = match expr $ \case
    App{} -> return True
    _     -> return False

isBlank :: NodeRef -> GraphOp Bool
-- isBlank expr = isJust <$> narrowTerm @IR.Blank expr
isBlank expr = match expr $ \case
    Blank{} -> return True
    _     -> return False

isLambda :: NodeRef -> GraphOp Bool
isLambda expr = match expr $ \case
    Lam{}     -> return True
    Grouped g -> source g >>= isLambda
    _         -> return False

isEnterable :: NodeRef -> GraphOp Bool
isEnterable expr = match expr $ \case
    Lam{}         -> return True
    ASGFunction{} -> return True
    Grouped g     -> source g >>= isEnterable
    _             -> return False

isMatch :: NodeRef -> GraphOp Bool
-- isMatch expr = isJust <$> narrowTerm @IR.Unify expr
isMatch expr = match expr $ \case
    Unify{} -> return True
    _     -> return False

isCons :: NodeRef -> GraphOp Bool
-- isCons expr = isJust <$> narrowTerm @IR.Cons expr
isCons expr = match expr $ \case
    Cons{} -> return True
    _     -> return False

isVar :: Layer.Reader (Component Nodes) IR.Model m => NodeRef -> m Bool
-- isVar expr = isJust <$> narrowTerm @IR.Var expr
isVar expr = match expr $ \case
    Var{} -> return True
    _     -> return False

isTuple :: NodeRef -> GraphOp Bool
-- isTuple expr = isJust <$> narrowTerm @IR.Tuple expr
isTuple expr = match expr $ \case
    Tuple{} -> return True
    _     -> return False

isASGFunction :: NodeRef -> ASTOp g Bool
-- isASGFunction expr = isJust <$> narrowTerm @IR.Function expr
isASGFunction expr = match expr $ \case
    ASGFunction{} -> return True
    _     -> return False

isRecord :: NodeRef -> GraphOp Bool
-- isRecord expr = isJust <$> narrowTerm @IR.Record expr
isRecord expr = match expr $ \case
    ClsASG{} -> return True
    _     -> return False

isNone :: NodeRef -> GraphOp Bool
isNone = flip matchExpr $ \case
    Cons n _ -> pure $ n == "None"
    _        -> pure False

isSeq :: NodeRef -> GraphOp Bool
isSeq = flip matchExpr $ \case
    Seq{} -> pure True
    _     -> pure False

isAnonymous :: NodeRef -> GraphOp Bool
isAnonymous expr = match expr $ \case
    Marked _ e -> isAnonymous =<< source e
    Unify _ _  -> return False
    _          -> return True

dumpPatternVars :: NodeRef -> GraphOp [NodeRef]
dumpPatternVars ref = match ref $ \case
    Var _     -> return [ref]
    Cons _ as -> fmap concat $ mapM (dumpPatternVars <=< source) =<< ptrListToList as
    Grouped g -> dumpPatternVars =<< source g
    Tuple a   -> fmap concat $ mapM (dumpPatternVars <=< source) =<< ptrListToList a
    _         -> return []

nodeIsPatternMatch :: NodeId -> GraphOp Bool
nodeIsPatternMatch nid = (do
    root <- getASTPointer nid
    varIsPatternMatch root) `catches` [
          Handler (\(_ :: NotUnifyException)         -> return False)
        , Handler (\(_ :: NodeDoesNotExistException) -> return False)
        ]

varIsPatternMatch :: NodeRef -> GraphOp Bool
varIsPatternMatch expr = do
    var <- getVarNode expr
    not <$> isVar var

rhsIsLambda :: NodeRef -> GraphOp Bool
rhsIsLambda ref = do
    rhs <- getTargetNode ref
    isLambda rhs

canEnterNode :: NodeRef -> GraphOp Bool
canEnterNode ref = do
    match' <- isMatch ref
    if match' then rhsIsLambda ref else return False

classFunctions :: NodeRef -> ClassOp [NodeRef]
classFunctions unit = do
    klass' <- classFromUnit unit
    match klass' $ \case
        ClsASG _ _ _ _ funs'' -> do
            funs <- ptrListToList funs''
            funs' <- mapM source funs
            catMaybes <$> forM funs' (\f -> cutThroughDocAndMarked f >>= \fun -> match fun $ \case
                ASGFunction{} -> return (Just f)
                _             -> return Nothing)

classFromUnit :: NodeRef -> ClassOp NodeRef
classFromUnit unit = match unit $ \case
    Unit _ _ c -> source c

getMetadataRef :: NodeRef -> ClassOp (Maybe NodeRef)
getMetadataRef unit = do
    klass' <- classFromUnit unit
    match klass' $ \case
        ClsASG _ _ _ _ funs'' -> do
            funs <- ptrListToList funs''
            funs' <- mapM source funs
            (Safe.headMay . catMaybes) <$> forM funs' (\f -> match f $ \case
                Metadata{} -> return (Just f)
                _          -> return Nothing)
        _ -> return Nothing

getFunByNodeId :: NodeId -> ClassOp NodeRef
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
