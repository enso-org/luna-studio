{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.GraphBuilder (
    buildNode
  , buildEdgeNodes
  , buildGraph
  , decodeBreadcrumbs
  , getEdgePortMapping
  , nodeConnectedToOutput
  ) where

import           Empire.Prelude

import           Control.Monad.State               hiding (when)

import qualified Data.List                         as List
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromJust, fromMaybe, maybeToList)
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
import           Empire.API.Data.TypeRep           (TypeRep(TLam, TStar))

import           Empire.ASTOp                      (ASTOp, runASTOp)
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Print               as Print
import qualified Empire.ASTOps.Read                as ASTRead
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import           Empire.Data.AST                   (NodeRef, astExceptionToException,
                                                    astExceptionFromException)
import           Empire.Data.Layers                (TypeLayer)
import           Empire.Empire

import           Luna.IR (match)
import qualified Luna.IR as IR
import           Luna.IR.Expr.Term.Uni

nameBreadcrumb :: ASTOp m => BreadcrumbItem -> m (Named BreadcrumbItem)
nameBreadcrumb item@(Breadcrumb.Lambda nid) = do
    name <- getNodeName nid
    return $ Named (fromMaybe "" name) item

decodeBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs (Breadcrumb items) = fmap Breadcrumb $ runASTOp $ forM items nameBreadcrumb

data CannotEnterNodeException = CannotEnterNodeException NodeId
    deriving Show
instance Exception CannotEnterNodeException where
    toException = astExceptionToException
    fromException = astExceptionFromException

throwIfCannotEnter :: ASTOp m => m ()
throwIfCannotEnter = do
    parent <- use Graph.insideNode
    case parent of
        Just node -> do
            canEnter <- canEnterNode node
            when (not canEnter) $ throwM $ CannotEnterNodeException node
        _ -> return ()

buildGraph :: ASTOp m => m API.Graph
buildGraph = do
    throwIfCannotEnter
    API.Graph <$> buildNodes <*> buildConnections

buildNodes :: ASTOp m => m [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- buildEdgeNodes
    nodes <- mapM buildNode allNodeIds
    return $ nodes ++ case edges of
        Just (inputEdge, outputEdge) -> [inputEdge, outputEdge]
        _                            -> []

type EdgeNodes = (API.Node, API.Node)

buildEdgeNodes :: ASTOp m => m (Maybe EdgeNodes)
buildEdgeNodes = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge  <- buildInputEdge inputPort
        outputEdge <- buildOutputEdge outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getOrCreatePortMapping :: ASTOp m => NodeId -> m (NodeId, NodeId)
getOrCreatePortMapping nid = do
    existingMapping <- uses Graph.breadcrumbPortMapping $ Map.lookup nid
    case existingMapping of
        Just m -> return m
        _      -> do
            ids <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
            Graph.breadcrumbPortMapping . at nid ?= ids
            return ids

getEdgePortMapping :: (MonadIO m, ASTOp m) => m (Maybe (NodeId, NodeId))
getEdgePortMapping = do
    lastBreadcrumbId <- use Graph.insideNode
    case lastBreadcrumbId of
        Just id' -> do
            isLambda <- AST.rhsIsLambda id'
            if isLambda
                then Just <$> getOrCreatePortMapping id'
                else return Nothing
        _ -> return Nothing

buildNode :: ASTOp m => NodeId -> m API.Node
buildNode nid = do
    root     <- GraphUtils.getASTPointer nid
    match'   <- ASTRead.isMatch root
    ref      <- if match' then GraphUtils.getASTTarget nid else return root
    expr     <- Print.printNodeExpression ref
    meta     <- AST.readMeta root
    name     <- fromMaybe "" <$> getNodeName nid
    canEnter <- canEnterNode nid
    ports <- buildPorts ref
    let code    = Nothing -- Just $ Text.pack expr
        portMap = Map.fromList $ flip fmap ports $ \p@(Port id' _ _ _) -> (id', p)
    return $ API.Node nid name (API.ExpressionNode $ Text.pack expr) canEnter portMap (fromMaybe def meta) code


canEnterNode :: ASTOp m => NodeId -> m Bool
canEnterNode nid = do
    root  <- GraphUtils.getASTPointer nid
    match' <- ASTRead.isMatch root
    if match' then AST.rhsIsLambda nid else return False

getNodeName :: ASTOp m => NodeId -> m (Maybe Text)
getNodeName nid = do
    root  <- GraphUtils.getASTPointer nid
    match' <- ASTRead.isMatch root
    if match' then do
        vnode <- GraphUtils.getASTVar nid
        name <- ASTRead.getVarName vnode
        return $ Just (Text.pack name)
    else return Nothing

getPortState :: ASTOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then return Connected else match node $ \case
        (IR.String s)   -> return . WithDefault . Constant . StringValue $ s
        IR.Integer i    -> return $ WithDefault $ Constant $ IntValue $ fromIntegral i
        IR.Rational r   -> return $ WithDefault $ Constant $ RationalValue r
        (Cons n) -> do
            name <- ASTRead.getName n
            case name of
                "False" -> return . WithDefault . Constant . BoolValue $ False
                "True"  -> return . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression <$> Print.printExpression node
        Blank   -> return NotConnected
        _     -> WithDefault . Expression <$> Print.printExpression node

extractArgTypes :: ASTOp m => NodeRef -> m [TypeRep]
extractArgTypes node = do
    match node $ \case
        Lam _args out -> do
            unpacked <- ASTDeconstruct.extractArguments node
            as     <- mapM Print.getTypeRep unpacked
            tailAs <- IR.source out >>= extractArgTypes
            return $ as ++ tailAs
        _ -> return []

extractPortInfo :: ASTOp m => NodeRef -> m ([TypeRep], [PortState])
extractPortInfo node = do
    match node $ \case
        App f _args -> do
            unpacked       <- ASTDeconstruct.extractArguments node
            portStates     <- mapM getPortState unpacked
            tp    <- do
                f' <- IR.source f
                foo <- IR.readLayer @TypeLayer f'
                IR.source foo
            types <- extractArgTypes tp
            return (types, portStates)
        Lam _as o -> do
            args     <- ASTDeconstruct.extractArguments node
            areBlank <- mapM ASTRead.isBlank args
            isApp    <- ASTRead.isApp =<< IR.source o
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
    let psCons = zipWith3  Port
                          (InPortId . Arg <$> [(0::Int)..]) (("arg " <>) . show <$> [(0::Int)..])
                          (types ++ replicate (length states - length types) TStar)
    return $ zipWith ($) psCons (states ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe Port)
buildSelfPort' seenAcc node = do
    let buildPort noType = do
            tpRep     <- if noType then return TStar else followTypeRep node
            portState <- getPortState node
            return . Just $ Port (InPortId Self) "self" tpRep portState

    match node $ \case
        (Acc _ t)  -> IR.source t >>= buildSelfPort' True
        (App t _)  -> IR.source t >>= buildSelfPort' seenAcc
        Lam _as o -> do
            args <- ASTDeconstruct.extractArguments node
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
    tp <- IR.source =<< IR.readLayer @TypeLayer ref
    Print.getTypeRep tp

buildPorts :: ASTOp m => NodeRef -> m [Port]
buildPorts ref = do
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep    <- followTypeRep ref
    outState <- getPortState ref
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) "Output" tpRep outState]

buildConnections :: ASTOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- getEdgePortMapping
    connections <- mapM (getNodeInputs edges) allNodes
    outputEdgeConnections <- forM edges $ uncurry getOutputEdgeInputs
    let foo = maybeToList $ join outputEdgeConnections
    return $ foo ++ concat connections

buildInputEdge :: ASTOp m => NodeId -> m API.Node
buildInputEdge nid = do
    Just lastb <- use Graph.insideNode
    ref   <- GraphUtils.getASTTarget lastb
    (types, _states) <- extractPortInfo ref
    argTypes <- case types of
        [] -> do
            numberOfArguments <- length <$> (extractArgTypes ref)
            return $ replicate numberOfArguments TStar
        _ -> return types
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

buildOutputEdge :: ASTOp m => NodeId -> m API.Node
buildOutputEdge nid = do
    Just lastb <- use Graph.insideNode
    ref <- GraphUtils.getASTTarget lastb
    out <- followTypeRep ref
    outputType <- case out of
        TLam _ t -> return t
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

getLambdaInputArgNumber :: ASTOp m => NodeRef -> m (Maybe Int)
getLambdaInputArgNumber lambda = do
    match lambda $ \case
        Lam _args out -> do
            out' <- IR.source out
            (out' `List.elemIndex`) <$> ASTDeconstruct.extractArguments lambda
        _ -> return Nothing

getOutputEdgeInputs :: ASTOp m => NodeId -> NodeId -> m (Maybe (OutPortRef, InPortRef))
getOutputEdgeInputs inputEdge outputEdge = do
    Just lambda <- use Graph.insideNode
    ref <- GraphUtils.getASTTarget lambda
    nid <- do
        outputIsInputNum <- getLambdaInputArgNumber ref
        case outputIsInputNum of
            Just index -> return $ Just (inputEdge, Projection index)
            _       -> do
                output <- ASTRead.getLambdaOutputRef ref
                nid <- ASTRead.getNodeId output
                case nid of
                    Just id' -> return $ Just (id', All)
                    _       -> return Nothing
    case nid of
        Just (id', arg) -> do
            return $ Just (OutPortRef id' arg, InPortRef outputEdge (Arg 0))
        _ -> return Nothing

nodeConnectedToOutput :: ASTOp m => m (Maybe NodeId)
nodeConnectedToOutput = do
    lambda <- use Graph.insideNode
    case lambda of
        Nothing -> return Nothing
        _       -> do
            edges <- getEdgePortMapping
            case edges of
                Just (i, o) -> do
                    connection <- getOutputEdgeInputs i o
                    case connection of
                        Nothing -> return Nothing
                        Just (OutPortRef nid _, _) -> return $ Just nid
                _           -> return Nothing


resolveInputNodeId :: ASTOp m => Maybe (NodeId, NodeId) -> [NodeRef] -> NodeRef -> m (Maybe NodeId)
resolveInputNodeId edgeNodes lambdaArgs ref = do
    nodeId <- ASTRead.getNodeId ref
    case List.find (== ref) lambdaArgs of
        Just _ -> return $ fmap fst edgeNodes
        _      -> return nodeId

getOuterLambdaArguments :: ASTOp m => m [NodeRef]
getOuterLambdaArguments = do
    lambda <- use Graph.insideNode
    case lambda of
        Just lambda' -> do
            ref <- GraphUtils.getASTTarget lambda'
            lambdaArgs <- ASTDeconstruct.extractArguments ref
            return lambdaArgs
        _ -> return []

getNodeInputs :: ASTOp m => Maybe (NodeId, NodeId) -> NodeId -> m [(OutPortRef, InPortRef)]
getNodeInputs edgeNodes nodeId = do
    root        <- GraphUtils.getASTPointer nodeId
    match'      <- ASTRead.isMatch root
    ref         <- if match' then GraphUtils.getASTTarget nodeId else return root
    selfMay     <- ASTRead.getSelfNodeRef ref
    lambdaArgs  <- getOuterLambdaArguments
    selfNodeMay <- case selfMay of
        Just self -> resolveInputNodeId edgeNodes lambdaArgs self
        Nothing   -> return Nothing
    let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args       <- ASTDeconstruct.extractArguments ref
    nodeMays   <- mapM (resolveInputNodeId edgeNodes lambdaArgs) args
    let withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ maybeToList selfConnMay ++ conns
