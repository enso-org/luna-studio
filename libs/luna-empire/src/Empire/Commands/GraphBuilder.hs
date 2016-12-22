{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.GraphBuilder where

import           Empire.Prelude

import           Control.Monad.Except              (throwError)
import           Control.Monad.State               hiding (when)

import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe, maybeToList)
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as Text
import qualified Data.UUID.V4                      as UUID (nextRandom)

import           Empire.API.Data.Breadcrumb        (Breadcrumb(..), BreadcrumbItem, Named(..))
import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph

import           Empire.API.Data.DefaultValue      (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph             as API
import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as API
import           Empire.API.Data.Port              (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep           (TypeRep(TLam))
import           Empire.API.Data.ValueType         (ValueType (..))

import           Empire.ASTOp                      (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Print               as Print
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import           Empire.Data.AST                   (NodeRef)
import           Empire.Data.Layers                (TypeLayer)
import           Empire.Empire

import           Luna.IR (match)
import qualified Luna.IR as IR
import           Luna.IR.Expr.Term.Uni

nameBreadcrumb :: BreadcrumbItem -> Command Graph (Named BreadcrumbItem)
nameBreadcrumb item@(Breadcrumb.Lambda nid) = do
    name <- runASTOp $ getNodeName nid
    return $ Named (fromMaybe "" name) item

decodeBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs (Breadcrumb items) = fmap Breadcrumb $ forM items nameBreadcrumb

buildGraph :: Command Graph API.Graph
buildGraph = do
    parent <- use Graph.insideNode
    canEnter <- runASTOp $ forM parent canEnterNode
    when (not $ fromMaybe True canEnter) $ throwError $ "cannot enter node " ++ show parent
    API.Graph <$> buildNodes <*> buildConnections

buildNodes :: Command Graph [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- buildEdgeNodes
    nodes <- mapM buildNode allNodeIds
    return $ nodes ++ case edges of
        Just (inputEdge, outputEdge) -> [inputEdge, outputEdge]
        _                            -> []

type EdgeNodes = (API.Node, API.Node)

buildEdgeNodes :: Command Graph (Maybe EdgeNodes)
buildEdgeNodes = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge <- buildInputEdge inputPort
        outputEdge <- buildOutputEdge outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getOrCreatePortMapping :: NodeId -> Command Graph (NodeId, NodeId)
getOrCreatePortMapping nid = do
    existingMapping <- uses Graph.breadcrumbPortMapping $ Map.lookup nid
    case existingMapping of
        Just m -> return m
        _      -> do
            ids <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
            Graph.breadcrumbPortMapping . at nid ?= ids
            return ids

getEdgePortMapping :: Command Graph (Maybe (NodeId, NodeId))
getEdgePortMapping = do
    lastBreadcrumbId <- use Graph.insideNode
    case lastBreadcrumbId of
        Just id' -> do
            isLambda <- runASTOp $ rhsIsLambda id'
            if isLambda
                then Just <$> getOrCreatePortMapping id'
                else return Nothing
        _ -> return Nothing

buildNode :: NodeId -> Command Graph API.Node
buildNode nid = runASTOp $ do
    root     <- GraphUtils.getASTPointer nid
    match'   <- isMatch root
    ref      <- if match' then GraphUtils.getASTTarget nid else return root
    expr     <- Print.printNodeExpression ref
    meta     <- AST.readMeta root
    name     <- fromMaybe "" <$> getNodeName nid
    canEnter <- canEnterNode nid
    ports <- buildPorts ref
    let code    = Nothing -- Just $ Text.pack expr
        portMap = Map.fromList $ flip fmap ports $ \p@(Port id' _ _ _) -> (id', p)
    return $ API.Node nid name (API.ExpressionNode $ Text.pack expr) canEnter portMap (fromMaybe def meta) code

isMatch :: ASTOp m => NodeRef -> m Bool
isMatch node = match node $ \case
    Unify{} -> return True
    _       -> return False

canEnterNode :: ASTOp m => NodeId -> m Bool
canEnterNode nid = do
    root  <- GraphUtils.getASTPointer nid
    match' <- isMatch root
    if match' then rhsIsLambda nid else return False

rhsIsLambda :: ASTOp m => NodeId -> m Bool
rhsIsLambda nid = do
    node <- GraphUtils.getASTTarget nid
    match node $ \case
        Lam{} -> return True
        _     -> return False

getNodeName :: ASTOp m => NodeId -> m (Maybe Text)
getNodeName nid = do
    root  <- GraphUtils.getASTPointer nid
    match' <- isMatch root
    if match' then do
        vnode <- GraphUtils.getASTVar nid
        match vnode $ \case
            Var n -> do
                name <- ASTBuilder.getName n
                return $ Just (Text.pack name)
    else return Nothing

getPortState :: ASTOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTBuilder.isGraphNode node
    if isConnected then return Connected else match node $ \case
        (IR.String s)   -> return . WithDefault . Constant . StringValue $ s
        IR.Integer i    -> return $ WithDefault $ Constant $ IntValue $ fromIntegral i
        IR.Rational r   -> return $ WithDefault $ Constant $ RationalValue r
        (Cons n) -> do
            name <- ASTBuilder.getName n
            case name of
                "False" -> return . WithDefault . Constant . BoolValue $ False
                "True"  -> return . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression <$> Print.printExpression node
        Blank   -> return NotConnected
        _     -> WithDefault . Expression <$> Print.printExpression node

extractArgTypes :: ASTOp m => NodeRef -> m [ValueType]
extractArgTypes node = do
    match node $ \case
        Lam _args out -> do
            unpacked <- ASTBuilder.unpackLamArguments node
            as     <- mapM getTypeRep unpacked
            tailAs <- IR.source out >>= extractArgTypes
            return $ as ++ tailAs
        _ -> return []

extractPortInfo :: ASTOp m => NodeRef -> m ([ValueType], [PortState])
extractPortInfo node = do
    match node $ \case
        App f _args -> do
            unpacked       <- ASTBuilder.dumpArguments node
            portStates     <- mapM getPortState unpacked
            tp    <- do
                f' <- IR.source f
                foo <- IR.readLayer @TypeLayer f'
                IR.source foo
            types <- extractArgTypes tp
            return (types, portStates)
        Lam _as o -> do
            args     <- ASTBuilder.unpackLamArguments node
            areBlank <- mapM ASTBuilder.isBlank args
            isApp    <- ASTBuilder.isApp =<< IR.source o
            if and areBlank && isApp
                then extractPortInfo =<< IR.source o
                else do
                    tpRef <- IR.source =<< IR.readLayer @TypeLayer node
                    types <- extractArgTypes tpRef
                    return (types, [])
        _ -> do
            tpRef <- IR.source =<< IR.readLayer @TypeLayer node
            types <- extractArgTypes tpRef
            return (types, [])

buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    (types, states) <- extractPortInfo ref
    let psCons = zipWith3 Port (InPortId . Arg <$> [(0::Int)..]) (("arg " <>) . show <$> [(0::Int)..]) (types ++ replicate (length states - length types) AnyType)
    return $ zipWith ($) psCons (states ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe Port)
buildSelfPort' seenAcc node = do
    let buildPort noType = do
            tpRep     <- if noType then return AnyType else followTypeRep node
            portState <- getPortState node
            return . Just $ Port (InPortId Self) "self" tpRep portState

    match node $ \case
        (Acc _ t)  -> IR.source t >>= buildSelfPort' True
        (App t _)  -> IR.source t >>= buildSelfPort' seenAcc
        Lam _as o -> do
            args <- ASTBuilder.unpackLamArguments node
            areBlank <- mapM ASTBuilder.isBlank args
            if and areBlank
                then IR.source o >>= buildSelfPort' seenAcc
                else if seenAcc then buildPort False else return Nothing
        Blank      -> return Nothing
        (Var _)    -> if seenAcc then buildPort False else buildPort True
        _        -> if seenAcc then buildPort False else return Nothing

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe Port)
buildSelfPort = buildSelfPort' False

followTypeRep :: ASTOp m => NodeRef -> m ValueType
followTypeRep ref = do
    tp <- IR.source =<< IR.readLayer @TypeLayer ref
    getTypeRep tp

getTypeRep :: ASTOp m => NodeRef -> m ValueType
getTypeRep tp = do
    rep <- Print.getTypeRep tp
    return $ TypeIdent rep

buildPorts :: ASTOp m => NodeRef -> m [Port]
buildPorts ref = do
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep    <- followTypeRep ref
    outState <- getPortState ref
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) "Output" tpRep outState]

buildConnections :: Command Graph [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- getEdgePortMapping
    connections <- mapM (getNodeInputs edges) allNodes
    outputEdgeConnections <- forM edges $ uncurry getOutputEdgeInputs
    let foo = maybeToList $ join outputEdgeConnections
    return $ foo ++ concat connections

buildInputEdge :: NodeId -> Command Graph API.Node
buildInputEdge nid = do
    lastb <- use Graph.insideNode <?!> "top-level nodes have no input edge"
    ref   <- runASTOp $ GraphUtils.getASTTarget lastb
    (types, _states) <- runASTOp $ extractPortInfo ref
    argTypes <- case types of
        [] -> do
            numberOfArguments <- length <$> (runASTOp $ extractArgTypes ref)
            return $ replicate numberOfArguments AnyType
        _ -> return types
    out <- runASTOp $ followTypeRep ref
    let nameGen = fmap (\i -> "input" ++ show i) [(0::Int)..]
        inputEdges = zipWith3 (\n t i -> Port (OutPortId $ Projection i) n t Port.NotConnected) nameGen argTypes [(0::Int)..]
    return $
        API.Node nid
            "inputEdge"
            API.InputEdge
            False
            (Map.fromList $ flip map inputEdges $ \port -> (port ^. Port.portId, port))
            def
            def

buildOutputEdge :: NodeId -> Command Graph API.Node
buildOutputEdge nid = do
    lastb <- use Graph.insideNode <?!> "top-level nodes have no output edge"
    ref <- runASTOp $ GraphUtils.getASTTarget lastb
    out <- runASTOp $ followTypeRep ref
    outputType <- case out of
        TypeIdent (TLam _ t) -> return $ TypeIdent t
        TypeIdent t -> return $ TypeIdent t
        a -> return a
    let port = Port (InPortId $ Arg 0) "output" outputType Port.NotConnected
    return $
        API.Node nid
            "outputEdge"
            API.OutputEdge
            False
            (Map.singleton (port ^. Port.portId) port)
            def
            def

getSelfNodeRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc node = do
    match node $ \case
        (Acc _ t) -> IR.source t >>= getSelfNodeRef' True
        (App t _) -> IR.source t >>= getSelfNodeRef' seenAcc
        _       -> return $ if seenAcc then Just node else Nothing

getSelfNodeRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getPositionalNodeRefs :: ASTOp m => NodeRef -> m [NodeRef]
getPositionalNodeRefs node = do
    match node $ \case
        App{} -> ASTBuilder.dumpArguments node
        _     -> return []

getLambdaOutputRef :: ASTOp m => NodeRef -> m NodeRef
getLambdaOutputRef node = do
    match node $ \case
        Lam _ out -> do
            nextLam <- IR.source out
            getLambdaOutputRef nextLam
        _         -> return node

getLambdaArgRefs :: ASTOp m => NodeRef -> m [NodeRef]
getLambdaArgRefs node = do
    match node $ \case
        Lam{} -> ASTBuilder.unpackLamArguments node
        _     -> return []

getLambdaInputArgNumber :: ASTOp m => NodeRef -> m (Maybe Int)
getLambdaInputArgNumber lambda = do
    match lambda $ \case
        Lam _args out -> do
            out' <- IR.source out
            (out' `List.elemIndex`) <$> ASTBuilder.unpackLamArguments lambda
        _ -> return Nothing

getOutputEdgeInputs :: NodeId -> NodeId -> Command Graph (Maybe (OutPortRef, InPortRef))
getOutputEdgeInputs inputEdge outputEdge = do
    lambda <- use Graph.insideNode <?!> "top-level nodes have no edges"
    ref <- runASTOp $ GraphUtils.getASTTarget lambda
    nid <- runASTOp $ do
        outputIsInputNum <- getLambdaInputArgNumber ref
        case outputIsInputNum of
            Just index -> return $ Just (inputEdge, Projection index)
            _       -> do
                output <- getLambdaOutputRef ref
                nid <- ASTBuilder.getNodeId output
                case nid of
                    Just id' -> return $ Just (id', All)
                    _       -> return Nothing
    case nid of
        Just (id', arg) -> do
            return $ Just (OutPortRef id' arg, InPortRef outputEdge (Arg 0))
        _ -> return Nothing

nodeConnectedToOutput :: Command Graph (Maybe NodeId)
nodeConnectedToOutput = do
    lambda <- use Graph.insideNode
    case lambda of
        Nothing -> return Nothing
        _       -> do
            (i, o) <- getEdgePortMapping <?!> "inside node so it's ok"
            connection <- getOutputEdgeInputs i o
            case connection of
                Nothing -> return Nothing
                Just (OutPortRef nid _, _) -> return $ Just nid


resolveInputNodeId :: Maybe (NodeId, NodeId) -> [NodeRef] -> NodeRef -> Command Graph (Maybe NodeId)
resolveInputNodeId edgeNodes lambdaArgs ref = runASTOp $ do
    nodeId <- ASTBuilder.getNodeId ref
    case List.find (== ref) lambdaArgs of
        Just _ -> return $ fmap fst edgeNodes
        _      -> return nodeId

getOuterLambdaArguments :: Command Graph [NodeRef]
getOuterLambdaArguments = runASTOp $ do
    lambda <- use Graph.insideNode
    case lambda of
        Just lambda' -> do
            ref <- GraphUtils.getASTTarget lambda'
            lambdaArgs <- getLambdaArgRefs ref
            return lambdaArgs
        _ -> return []

getNodeInputs :: Maybe (NodeId, NodeId) -> NodeId -> Command Graph [(OutPortRef, InPortRef)]
getNodeInputs edgeNodes nodeId = do
    root        <- runASTOp $ GraphUtils.getASTPointer nodeId
    match'      <- runASTOp $ isMatch root
    ref         <- if match' then runASTOp $ GraphUtils.getASTTarget nodeId else return root
    selfMay     <- runASTOp $ getSelfNodeRef ref
    lambdaArgs  <- getOuterLambdaArguments
    selfNodeMay <- case selfMay of
        Just self -> resolveInputNodeId edgeNodes lambdaArgs self
        Nothing   -> return Nothing
    let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args       <- runASTOp $ getPositionalNodeRefs ref
    nodeMays   <- mapM (resolveInputNodeId edgeNodes lambdaArgs) args
    let withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ maybeToList selfConnMay ++ conns
