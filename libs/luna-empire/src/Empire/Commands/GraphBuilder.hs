{-# LANGUAGE LambdaCase          #-}
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
  , buildInputEdge
  , buildInputEdgeTypecheckUpdate
  , buildOutputSidebarTypecheckUpdate
  , decodeBreadcrumbs
  , getEdgePortMapping
  , getNodeIdSequence
  , getInPortDefault
  , getDefault
  , getNodeName
  , nodeConnectedToOutput
  ) where

import           Empire.Prelude

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
import           Empire.API.Data.Port              (InPort (..), _InPortId, OutPort (..), Port (..), PortId (..), OutPortTree (..), PortState (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..), srcNodeId)
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
    nodes <- buildNodes
    edges <- buildEdgeNodes connections
    let allNodes = nodes ++ case edges of
            Just (input, output) -> [input, output]
            _                    -> []
    API.Graph allNodes connections <$> buildMonads

buildNodes :: ASTOp m => m [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes <- mapM buildNode allNodeIds
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

type EdgeNodes = (API.Node, API.Node)

buildEdgeNodes :: ASTOp m => [(OutPortRef, InPortRef)] -> m (Maybe EdgeNodes)
buildEdgeNodes connections = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge  <- buildInputEdge connections inputPort
        outputEdge <- buildOutputSidebar connections outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getEdgePortMapping :: (MonadIO m, ASTOp m) => m (Maybe (NodeId, NodeId))
getEdgePortMapping = preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping

buildNode :: ASTOp m => NodeId -> m API.Node
buildNode nid = do
    root     <- GraphUtils.getASTPointer nid
    match'   <- ASTRead.isMatch root
    ref      <- if match' then GraphUtils.getASTTarget nid else return root
    expr     <- Print.printNodeExpression ref
    meta     <- AST.readMeta root
    name     <- fromMaybe "" <$> getNodeName nid
    canEnter <- ASTRead.canEnterNode root
    inports  <- buildInPorts  root
    outports <- buildOutPorts root
    let code    = Just $ Text.pack expr
        portMap = Map.fromList $ flip fmap inports $ \p@(Port (InPortId id') _ _ _) -> (id', p)
    return $ API.Node nid name (API.ExpressionNode $ Text.pack expr) canEnter portMap outports (fromMaybe def meta) code

buildNodeTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root   <- GraphUtils.getASTPointer nid
  match' <- ASTRead.isMatch root
  ref    <- if match' then GraphUtils.getASTTarget nid else return root
  ports  <- buildPorts ref
  let portMap = Map.fromList $ flip fmap ports $ \p@(Port id' _ _ _) -> (id', p)
  return $ API.NodeTypecheckerUpdate nid portMap

getUniName :: ASTOp m => NodeRef -> m (Maybe Text)
getUniName root = do
    match' <- ASTRead.isMatch root
    if match' then do
        vnode <- ASTRead.getVarNode root
        name <- match vnode $ \case
            Var{}  -> ASTRead.getVarName vnode
            _      -> Print.printExpression vnode
        return $ Just (Text.pack name)
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

buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    typed <- extractPortInfo ref
    names <- getPortsNames ref
    let portsTypes = fmap fst typed ++ replicate (length names - length typed) TStar
        psCons = zipWith3 Port
                          (InPortId . Arg <$> [(0::Int)..])
                          (names ++ (("arg" ++) . show <$> [0..]))
                          portsTypes
    return $ zipWith ($) psCons (fmap snd typed ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe Port)
buildSelfPort' seenAcc node = do
    let buildPort noType = do
            tpRep     <- if noType then return TStar else followTypeRep node
            portState <- getPortState node
            return . Just $ Port (InPortId Self) "self" tpRep portState

    match node $ \case
        (Acc t _)  -> IR.source t >>= buildSelfPort' True
        (App t _)  -> IR.source t >>= buildSelfPort' seenAcc
        Lam _as o -> do
            args     <- ASTDeconstruct.extractArguments node
            areBlank <- mapM ASTRead.isBlank args
            if and areBlank
                then IR.source o >>= buildSelfPort' seenAcc
                else if seenAcc then buildPort False else return Nothing
        Blank      -> return Nothing
        (Var _)    -> if seenAcc then buildPort False else buildPort True
        _        -> if seenAcc then buildPort False else return Nothing

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe Port)
buildSelfPort = buildSelfPort' False

followTypeRep :: ASTOp m => NodeRef -> m TypeRep
followTypeRep ref = do
    tp <- IR.source =<< IR.getLayer @TypeLayer ref
    Print.getTypeRep tp

buildPorts :: ASTOp m => NodeRef -> m [Port]
buildPorts node = do
    isMatch  <- ASTRead.isMatch node
    flipNode <- if isMatch then ASTRead.varIsPatternMatch node else return False
    ref      <- if isMatch then if flipNode then ASTRead.getVarNode node else ASTRead.getTargetNode node
                           else return node
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep    <- followTypeRep ref
    outState <- getPortState ref
    let ports = selfPort ++ argPorts ++ [Port (OutPortId All) "Output" tpRep outState]
    return $ if flipNode then flipPorts ports else ports

data SelfPortFlipException = SelfPortFlipException
    deriving Show

instance Exception SelfPortFlipException where
    fromException = astExceptionFromException
    toException   = astExceptionToException

flipPorts :: [Port] -> [Port]
flipPorts = map flipPort
    where
        flipPort (Port portId portName portType portState) =
            Port (flipPortId portId) (flipPortName portName) portType portState

        flipPortId (OutPortId All)              = InPortId  (Arg 0)
        flipPortId (OutPortId (Projection i _)) = InPortId  (Arg i)
        flipPortId (InPortId  (Arg i))          = OutPortId (Projection i All)
        flipPortId (InPortId  Self)             = error "flipPortId: cannot flip self"

        flipPortName "Output" = "Input"
        flipPortName name     = name

buildConnections :: ASTOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    edges <- getEdgePortMapping
    connections <- mapM (getNodeInputs edges) allNodes
    outputEdgeConnections <- forM edges $ uncurry getOutputSidebarInputs
    let foo = maybeToList $ join outputEdgeConnections
    return $ foo ++ concat connections

buildInputEdgeTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildInputEdgeTypecheckUpdate nid = do
    API.Node nid _ _ _ m _ _ <- buildInputEdge [] nid
    return $ API.NodeTypecheckerUpdate nid m


buildInputEdge :: ASTOp m => [(OutPortRef, InPortRef)] -> NodeId -> m API.Node
buildInputEdge connections nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    tp       <- IR.getLayer @TypeLayer ref >>= IR.source
    types    <- extractArgTypes tp
    let connectedPorts = map (\(OutPortRef _ (Projection p _)) -> p)
               $ map fst
               $ filter (\(OutPortRef refNid p,_) -> nid == refNid ^. NodeLoc.nodeId)
               $ connections
        states = map (\i -> if i `elem` connectedPorts then Connected else NotConnected) [(0::Int)..]
    names <- getPortsNames ref
    argTypes <- case types of
        [] -> do
            args <- ASTDeconstruct.extractArguments ref
            vars <- concat <$> mapM ASTRead.getVarsInside args
            let numberOfPorts = length vars
            return $ replicate numberOfPorts TStar
        _  -> return types
    let inputEdges = List.zipWith4 (\n t state i -> Port (OutPortId $ Projection i All) n t state) names argTypes states [(0::Int)..]
    return $
        API.Node nid
            "inputEdge"
            (API.InputEdge $ flip OutPortTree [] <$> inputEdges)
            False
            (Map.fromList $ flip map inputEdges $ \port -> (port ^. Port.portId, port))
            def
            def

buildOutputSidebarTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid m <- buildOutputSidebar [] nid
    return $ API.NodeTypecheckerUpdate nid m

buildOutputSidebar :: ASTOp m => [(OutPortRef, InPortRef)] -> NodeId -> m API.OutputSidebar
buildOutputSidebar connections nid = do
    Just ref <- ASTRead.getCurrentASTTarget
    out      <- followTypeRep ref
    let traverseLams (TLam _ t) = traverseLams t
        traverseLams s          = s
        outputType = traverseLams out
    let connectedPorts = map (\(InPortRef _ (Arg p)) -> p)
             $ map snd
             $ filter (\(_, InPortRef refNid p) -> nid == refNid ^. NodeLoc.nodeId)
             $ connections
        outputConnected = if 0 `elem` connectedPorts then Connected else NotConnected
        port = Port (InPortId $ Arg 0) "output" outputType outputConnected
    return $
        API.OutputSidebar nid (Map.singleton (Arg 0) port)

getLambdaInputArgNumber :: ASTOp m => NodeRef -> m (Maybe Int)
getLambdaInputArgNumber lambda = do
    match lambda $ \case
        Grouped g      -> IR.source g >>= getLambdaInputArgNumber
        Lam _arg _body -> do
            out' <- ASTRead.getLambdaOutputRef lambda
            args <- ASTDeconstruct.extractArguments lambda
            vars <- concat <$> mapM ASTRead.getVarsInside args
            return $ out' `List.elemIndex` vars
        _ -> return Nothing

getOutputSidebarInputs :: ASTOp m => NodeId -> NodeId -> m (Maybe (OutPortRef, InPortRef))
getOutputSidebarInputs inputEdge outputEdge = do
    Just ref <- ASTRead.getCurrentASTTarget
    nid <- do
        outputIsInputNum <- getLambdaInputArgNumber ref
        case outputIsInputNum of
            Just index -> return $ Just (inputEdge, Projection index All)
            _       -> do
                output <- ASTRead.getLambdaOutputRef ref
                nid <- ASTRead.getNodeId output
                case nid of
                    Just id' -> return $ Just (id', All)
                    _       -> return Nothing
    case nid of
        Just (id', arg) -> do
            return $ Just (OutPortRef (NodeLoc def id') arg, InPortRef (NodeLoc def outputEdge) (Arg 0))
        _ -> return Nothing

nodeConnectedToOutput :: ASTOp m => m (Maybe NodeId)
nodeConnectedToOutput = do
    edges  <- preuse $ Graph.breadcrumbHierarchy . BH._LambdaParent . BH.portMapping
    fmap join $ forM edges $ \(i, o) -> do
        connection <- getOutputSidebarInputs i o
        return $ (view $ _1 . srcNodeId) <$> connection

resolveInputNodeId :: ASTOp m => Maybe (NodeId, NodeId) -> [NodeRef] -> NodeRef -> m (Maybe OutPort, Maybe NodeId)
resolveInputNodeId edgeNodes lambdaArgs ref = do
    case List.findIndex (== ref) lambdaArgs of
        Just i -> return (Just $ Projection i All, fmap fst edgeNodes)
        _      -> do
            projection <- IR.getLayer @Marker ref
            case projection of
                Just (OutPortRef nodeLoc portId) -> return (Just portId, Just $ nodeLoc ^. NodeLoc.nodeId)
                _                                -> return (Nothing, Nothing)

getOuterLambdaArguments :: ASTOp m => m [NodeRef]
getOuterLambdaArguments = do
    refMay <- ASTRead.getCurrentASTTarget
    case refMay of
        Just ref -> ASTDeconstruct.extractArguments ref
        _        -> return []

outIndexToProjection :: Maybe Int -> OutPort
outIndexToProjection Nothing = All
outIndexToProjection (Just i) = Projection i All

getNodeInputs :: ASTOp m => Maybe (NodeId, NodeId) -> NodeId -> m [(OutPortRef, InPortRef)]
getNodeInputs edgeNodes nodeId = do
    root        <- GraphUtils.getASTPointer nodeId
    match'      <- ASTRead.isMatch root
    ref         <- if match' then GraphUtils.getASTTarget nodeId else return root
    isPattern   <- if match' then ASTRead.varIsPatternMatch root else return False
    pattern     <- if isPattern then Just <$> GraphUtils.getASTVar nodeId
                                else return Nothing
    case pattern of
        Just _p -> do
            nodeBeingMatched <- GraphUtils.getASTTarget nodeId >>= ASTRead.getNodeId
            case nodeBeingMatched of
                Just id' -> return [(OutPortRef (NodeLoc def id') Port.All, InPortRef (NodeLoc def nodeId) (Port.Arg 0))]
                _        -> return []
        _       -> do
            selfMay     <- ASTRead.getSelfNodeRef ref
            lambdaArgs  <- getOuterLambdaArguments
            selfNodeMay <- fmap join $ forM selfMay $ fmap snd . resolveInputNodeId edgeNodes lambdaArgs
            let projection  = case selfMay of
                    Just self -> Just $ outIndexToProjection $ List.findIndex (== self) lambdaArgs
                    Nothing   -> Nothing
            let selfConnMay = (,) <$> (OutPortRef <$> (NodeLoc def <$> selfNodeMay) <*> projection)
                                  <*> (Just $ InPortRef (NodeLoc def nodeId) Self)

            args     <- ASTDeconstruct.extractArguments ref
            nodeMays <- mapM (resolveInputNodeId edgeNodes lambdaArgs) args
            let withInd  = zipWith (\(outPortIndex, nodeId) index -> (outPortIndex, nodeId, index)) nodeMays [0..]
                hasNodeId (outIndex, Just nodeId, index) = Just (outIndex, nodeId, index)
                hasNodeId _ = Nothing
                onlyExt  = catMaybes $ map hasNodeId withInd
                conns    = flip map onlyExt $ \((fromMaybe All -> proj), n, i) -> (OutPortRef (NodeLoc def n) proj, InPortRef (NodeLoc def nodeId) (Arg i))
            return $ maybeToList selfConnMay ++ conns
