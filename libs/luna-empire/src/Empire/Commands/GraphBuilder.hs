{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.GraphBuilder (
    buildConnections
  , buildMonads
  , buildNode
  , buildNodeTypecheckUpdate
  , buildNodes
  , buildEdgeNodes
  , buildGraph
  , buildInputSidebar
  , buildInputSidebarTypecheckUpdate
  , buildOutputSidebarTypecheckUpdate
  , decodeBreadcrumbs
  , getEdgePortMapping
  , getNodeIdSequence
  , getInPortDefault
  , getDefault
  , getNodeName
  , nodeConnectedToOutput
  ) where

import           Empire.Prelude                    hiding (toList)
import           Data.Foldable                     (toList)

import           Control.Monad.State               hiding (when)

import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, isJust, fromJust, fromMaybe, maybeToList)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.UUID.V4                      as UUID (nextRandom)

import           Empire.API.Data.Breadcrumb        (Breadcrumb(..), BreadcrumbItem, Named(..))
import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import qualified Empire.Data.BreadcrumbHierarchy   as BH
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph

import           Empire.API.Data.PortDefault       (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph             as API
import           Empire.API.Data.MonadPath              (MonadPath(MonadPath))
import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as API
import qualified Empire.API.Data.NodeLoc           as NodeLoc
import           Empire.API.Data.NodeLoc           (NodeLoc (..))
import           Empire.API.Data.Port              (InPortIndex (..), OutPortIndex (..), OutPort, InPort, InPortId, OutPortId, OutPorts (..), InPorts (..), Port (..), InPortTree, OutPortTree, PortState (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.LabeledTree       (LabeledTree (..))
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..), srcNodeId, dstNodeId)
import           Empire.API.Data.TypeRep           (TypeRep(TLam, TStar, TCons))

import           Empire.ASTOp                      (ASTOp, match, runASTOp)
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Print               as Print
import qualified Empire.ASTOps.Read                as ASTRead
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import           Empire.Data.AST                   (NodeRef, astExceptionToException,
                                                    astExceptionFromException, NotUnifyException, NotAppException (..))
import           Empire.Data.Layers                (Marker, TypeLayer)
import           Empire.Empire

import qualified Luna.IR as IR
import qualified OCI.IR.Combinators   as IR
import           Luna.IR.Term.Uni
import qualified Luna.IR.Term.Literal as Lit

decodeBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs bs@(Breadcrumb items) = runASTOp $ do
    bh    <- use Graph.breadcrumbHierarchy
    names <- forM (BH.getBreadcrumbItems bh bs) $ \child -> getUniName $ child ^. BH.self . BH.anyRef
    return $ Breadcrumb $ fmap (\(n, i) -> Named (fromMaybe "" n) i) $ zip names items

data CannotEnterNodeException = CannotEnterNodeException NodeId
    deriving Show
instance Exception CannotEnterNodeException where
    toException = astExceptionToException
    fromException = astExceptionFromException

buildGraph :: ASTOp m => m API.Graph
buildGraph = do
    connections <- buildConnections
    nodes       <- buildNodes
    edges       <- buildEdgeNodes
    API.Graph nodes connections (fst <$> edges) (snd <$> edges) <$> buildMonads

buildNodes :: ASTOp m => m [API.ExpressionNode]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNode allNodeIds
    return nodes

buildMonads :: ASTOp m => m [MonadPath]
buildMonads = do
    allNodeIds <- getNodeIdSequence
    ioPath     <- filterM doesIO allNodeIds
    let ioMonad = MonadPath (TCons "IO" []) ioPath
    return [ioMonad]

doesIO :: ASTOp m => NodeId -> m Bool
doesIO node = do
    ref <- ASTRead.getASTPointer node
    tp  <- IR.getLayer @TypeLayer ref >>= IR.source
    IR.matchExpr tp $ \case
        Monadic _ m -> hasIO =<< IR.source m
        _           -> return False

hasIO :: ASTOp m => NodeRef -> m Bool
hasIO ref = IR.matchExpr ref $ \case
    Cons n _  -> return $ n == "IO"
    Unify l r -> (||) <$> (hasIO =<< IR.source l) <*> (hasIO =<< IR.source r)
    _         -> return False


nodeIdInsideLambda :: ASTOp m => NodeRef -> m (Maybe NodeId)
nodeIdInsideLambda node = (ASTRead.getVarNode node >>= ASTRead.getNodeId) `catch`
    (\(_e :: NotUnifyException) -> return Nothing)

getNodeIdSequence :: ASTOp m => m [NodeId]
getNodeIdSequence = do
    lref <- ASTRead.getCurrentASTTarget
    nodeSeq <- do
        bodySeq <- case lref of
            Just l -> ASTRead.getLambdaBodyRef l
            _      -> preuse $ Graph.breadcrumbHierarchy . BH.body
        case bodySeq of
            Just b -> AST.readSeq b
            _      -> return []
    catMaybes <$> mapM nodeIdInsideLambda nodeSeq

type EdgeNodes = (API.InputSidebar, API.OutputSidebar)

buildEdgeNodes :: ASTOp m => m (Maybe EdgeNodes)
buildEdgeNodes = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge  <- buildInputSidebar  inputPort
        outputEdge <- buildOutputSidebar outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getEdgePortMapping :: (MonadIO m, ASTOp m) => m (Maybe (NodeId, NodeId))
getEdgePortMapping = preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping

buildNode :: ASTOp m => NodeId -> m API.ExpressionNode
buildNode nid = do
    root      <- GraphUtils.getASTPointer nid
    ref       <- GraphUtils.getASTTarget  nid
    expr      <- Text.pack <$> Print.printNodeExpression ref
    meta      <- fromMaybe def <$> AST.readMeta root
    name      <- getNodeName nid
    canEnter  <- ASTRead.isLambda ref
    inports   <- buildInPorts  ref
    outports  <- buildOutPorts root
    let code      = Just expr
    return $ API.ExpressionNode nid expr name code inports outports meta canEnter

buildNodeTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root     <- GraphUtils.getASTPointer nid
  ref      <- GraphUtils.getASTTarget  nid
  inPorts  <- buildInPorts  ref
  outPorts <- buildOutPorts root
  return $ API.ExpressionUpdate nid inPorts outPorts

getUniName :: ASTOp m => NodeRef -> m (Maybe Text)
getUniName root = do
    match' <- ASTRead.isMatch root
    if match' then do
        vnode <- ASTRead.getVarNode root
        Just . Text.pack <$> Print.printExpression vnode
    else return Nothing

getNodeName :: ASTOp m => NodeId -> m (Maybe Text)
getNodeName nid = ASTRead.getASTPointer nid >>= getUniName

getDefault :: ASTOp m => NodeRef -> m PortDefault
getDefault arg = match arg $ \case
        IR.String s       -> return $ Constant $ StringValue $ s
        IR.Number i       -> return $ Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else DoubleValue $ Lit.toDouble i
        IR.Cons "True"  _ -> return $ Constant $ BoolValue True
        IR.Cons "False" _ -> return $ Constant $ BoolValue False
        _                 -> Expression <$> Print.printExpression arg

getInPortDefault :: ASTOp m => NodeRef -> Int -> m PortDefault
getInPortDefault ref pos = do
    (_, args) <- ASTDeconstruct.deconstructApp ref
    argRef    <- maybe (throwM $ NotAppException ref) return $ args ^? ix pos
    getDefault argRef


getPortState :: ASTOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then return Connected else match node $ \case
        IR.String s     -> return . WithDefault . Constant . StringValue $ s
        IR.Number i     -> return . WithDefault . Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else DoubleValue $ Lit.toDouble i
        Cons n _ -> do
            name <- pure $ pathNameToString n
            case name of
                "False" -> return . WithDefault . Constant . BoolValue $ False
                "True"  -> return . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression <$> Print.printExpression node
        Blank -> return NotConnected
        _     -> WithDefault . Expression <$> Print.printExpression node

extractArgTypes :: ASTOp m => NodeRef -> m [TypeRep]
extractArgTypes node = do
    match node $ \case
        Monadic s _ -> extractArgTypes =<< IR.source s
        Lam arg out -> (:) <$> (Print.getTypeRep =<< IR.source arg) <*> (extractArgTypes =<< IR.source out)
        _           -> return []

safeGetVarName :: ASTOp m => NodeRef -> m (Maybe String)
safeGetVarName node = do
    name <- (Just <$> ASTRead.getVarName node) `catch`
        (\(e :: ASTRead.NoNameException) -> return Nothing)
    return name

extractArgNames :: ASTOp m => NodeRef -> m [Maybe String]
extractArgNames node = do
    match node $ \case
        Grouped g -> IR.source g >>= extractArgNames
        Lam{}  -> do
            insideLam  <- insideThisNode node
            args       <- ASTDeconstruct.extractArguments node
            vars       <- concat <$> mapM ASTRead.getVarsInside args
            let ports = if insideLam then vars else args
            mapM safeGetVarName ports
        -- App is Lam that has some args applied
        App{}  -> extractAppArgNames node
        Cons{} -> do
            vars  <- ASTRead.getVarsInside node
            names <- mapM ASTRead.getVarName vars
            return $ map Just names
        _ -> return []

extractAppArgNames :: ASTOp m => NodeRef -> m [Maybe String]
extractAppArgNames node = go [] node
    where
        go vars node = match node $ \case
            App f a -> do
                varName <- safeGetVarName =<< IR.source a
                go (varName : vars) =<< IR.source f
            Lam{}   -> extractArgNames node
            Cons{}  -> return vars
            Var{}   -> return vars
            Acc{}   -> return vars
            _       -> return []

insideThisNode :: ASTOp m => NodeRef -> m Bool
insideThisNode node = do
    curr <- ASTRead.getCurrentASTTarget
    return $ case curr of
        Just n -> n == node
        _      -> False

getPortsNames :: ASTOp m => NodeRef -> m [String]
getPortsNames node = do
    names <- extractArgNames node
    let backupNames = map (\i -> "arg" ++ show i) [(0::Int)..]
    forM (zip names backupNames) $ \(name, backup) -> case name of
        Just n -> return n
        _      -> return backup

extractAppliedPorts :: ASTOp m => Bool -> Bool -> [NodeRef] -> NodeRef -> m [Maybe (TypeRep, PortState)]
extractAppliedPorts seenApp  seenLam bound node = IR.matchExpr node $ \case
    Lam i o -> case seenApp of
        True  -> return []
        False -> do
            inp <- IR.source i
            out <- IR.source o
            extractAppliedPorts False True (inp : bound) out
    App f a -> case seenLam of
        True  -> return []
        False -> do
            arg          <- IR.source a
            isB          <- ASTRead.isBlank arg
            argTp        <- IR.getLayer @TypeLayer arg >>= IR.source
            res          <- if isB || elem arg bound then return Nothing else Just .: (,) <$> Print.getTypeRep argTp <*> getPortState arg
            rest         <- extractAppliedPorts True False bound =<< IR.source f
            return $ res : rest
    _       -> return []


fromMaybePort :: Maybe (TypeRep, PortState) -> (TypeRep, PortState)
fromMaybePort Nothing  = (TStar, NotConnected)
fromMaybePort (Just p) = p

mergePortInfo :: [Maybe (TypeRep, PortState)] -> [TypeRep] -> [(TypeRep, PortState)]
mergePortInfo []             []       = []
mergePortInfo (p : rest)     []       = fromMaybePort p : mergePortInfo rest []
mergePortInfo []             (t : ts) = (t, NotConnected) : mergePortInfo [] ts
mergePortInfo (Nothing : as) (t : ts) = (t, NotConnected) : mergePortInfo as ts
mergePortInfo (Just a  : as) ts       = a : mergePortInfo as ts

extractPortInfo :: ASTOp m => NodeRef -> m [(TypeRep, PortState)]
extractPortInfo n = do
    applied  <- reverse <$> extractAppliedPorts False False [] n
    tp       <- IR.getLayer @TypeLayer n >>= IR.source
    fromType <- extractArgTypes tp
    return $ mergePortInfo applied fromType

buildArgPorts :: ASTOp m => NodeRef -> m [InPort]
buildArgPorts ref = do
    typed <- extractPortInfo ref
    names <- getPortsNames ref
    let portsTypes = fmap fst typed ++ replicate (length names - length typed) TStar
        psCons = zipWith3 Port
                          (pure . Arg <$> [(0::Int)..])
                          (names ++ (("arg" ++) . show <$> [0..]))
                          portsTypes
    return $ zipWith ($) psCons (fmap snd typed ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe InPort)
buildSelfPort' seenAcc node = do
    let buildActualSelf = do
            tpRep     <- followTypeRep node
            portState <- getPortState  node
            return $ Just $ Port [Self] "self" tpRep portState
    let potentialSelf = Just $ Port [Self] "self" TStar NotConnected

    match node $ \case
        (Acc t _)  -> IR.source t >>= buildSelfPort' True
        (App t _)  -> IR.source t >>= buildSelfPort' seenAcc
        Blank      -> return Nothing
        (Var _)    -> if seenAcc then buildActualSelf else return potentialSelf
        _          -> if seenAcc then buildActualSelf else return Nothing

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe InPort)
buildSelfPort = buildSelfPort' False

buildWholePort :: ASTOp m => NodeRef -> m InPort
buildWholePort ref = do
    tp    <- followTypeRep ref
    state <- getPortState ref
    return $ Port [] "base" tp state

followTypeRep :: ASTOp m => NodeRef -> m TypeRep
followTypeRep ref = do
    tp <- IR.source =<< IR.getLayer @TypeLayer ref
    Print.getTypeRep tp

buildInPorts :: ASTOp m => NodeRef -> m (InPortTree InPort)
buildInPorts ref = do
    selfPort <- buildSelfPort ref
    argPorts <- buildArgPorts ref
    whole    <- buildWholePort ref
    return $ LabeledTree (InPorts (LabeledTree def <$> selfPort) (LabeledTree def <$> argPorts)) whole


buildDummyOutPort :: ASTOp m => NodeRef -> m (OutPortTree OutPort)
buildDummyOutPort ref = do
    tp <- followTypeRep ref
    return $ LabeledTree (Port.OutPorts []) (Port [] "Output" tp NotConnected)

buildOutPortTree :: ASTOp m => OutPortId -> NodeRef -> m (OutPortTree OutPort)
buildOutPortTree portId ref' = do
    ref   <- ASTRead.cutThroughGroups ref'
    name  <- Print.printExpression ref
    tp    <- followTypeRep ref
    let wholePort = Port portId name tp NotConnected
    children <- match ref $ \case
        Cons _ as -> zipWithM buildOutPortTree ((portId ++) . pure . Port.Projection <$> [0 ..]) =<< mapM IR.source as
        _         -> return []
    return $ LabeledTree (OutPorts children) wholePort

buildOutPorts :: ASTOp m => NodeRef -> m (OutPortTree OutPort)
buildOutPorts ref = match ref $ \case
    Unify l r -> buildOutPortTree [] =<< IR.source l
    _         -> buildDummyOutPort ref


buildConnections :: ASTOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes       <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    edges          <- getEdgePortMapping
    connections    <- mapM getNodeInputs allNodes
    outputEdgeConn <- forM (snd <$> edges) getOutputSidebarInputs
    return $ (maybeToList $ join outputEdgeConn) ++ concat connections

buildInputSidebarTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildInputSidebarTypecheckUpdate nid = do
    API.InputSidebar nid ps <- buildInputSidebar nid
    return $ API.InputSidebarUpdate nid ps


buildInputSidebar :: ASTOp m => NodeId -> m API.InputSidebar
buildInputSidebar nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    args     <- ASTDeconstruct.extractArguments ref
    argTrees <- zipWithM buildOutPortTree (pure . Projection <$> [0..]) args
    return $ API.InputSidebar nid argTrees

buildOutputSidebarTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid m <- buildOutputSidebar nid
    return $ API.OutputSidebarUpdate nid m

buildOutputSidebar :: ASTOp m => NodeId -> m API.OutputSidebar
buildOutputSidebar nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    out      <- ASTRead.getLambdaOutputRef ref
    tp       <- followTypeRep out
    state    <- getPortState  out
    return $ API.OutputSidebar nid $ LabeledTree (Port.InPorts Nothing [])  $ Port [] "output" tp state

getOutputSidebarInputs :: ASTOp m => NodeId -> m (Maybe (OutPortRef, InPortRef))
getOutputSidebarInputs outputEdge = do
    Just ref <- ASTRead.getCurrentASTTarget
    out      <- ASTRead.getLambdaOutputRef ref
    wholeIn  <- resolveInput out
    return $ (, InPortRef (NodeLoc def outputEdge) []) <$> wholeIn

nodeConnectedToOutput :: ASTOp m => m (Maybe NodeId)
nodeConnectedToOutput = do
    edges  <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
    fmap join $ forM edges $ \(i, o) -> do
        connection <- getOutputSidebarInputs o
        return $ (view $ _1 . srcNodeId) <$> connection

resolveInput :: ASTOp m => NodeRef -> m (Maybe OutPortRef)
resolveInput = IR.getLayer @Marker

getNodeInputs :: ASTOp m => NodeId -> m [(OutPortRef, InPortRef)]
getNodeInputs nid = do
    ref      <- ASTRead.getASTTarget   nid
    self     <- ASTRead.getSelfNodeRef ref
    args     <- ASTDeconstruct.extractArguments ref
    selfIn   <- catMaybes . toList <$> mapM resolveInput self
    argsIn   <- mapM resolveInput args
    wholeIn  <- filter ((/= nid) . view srcNodeId) . toList <$> resolveInput ref
    let loc      = NodeLoc def nid
        selfConn  = (, InPortRef loc [Self]) <$> selfIn
        argsConn  = catMaybes $ zipWith (\p c -> (, InPortRef loc [Arg p]) <$> c) [0..] argsIn
        wholeConn = (, InPortRef loc []) <$> wholeIn
    return $ selfConn ++ wholeConn ++ argsConn
