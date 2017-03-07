{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.GraphBuilder (
    buildConnections
  , buildNode
  , buildNodeTypecheckUpdate
  , buildNodes
  , buildEdgeNodes
  , buildGraph
  , buildInputEdge
  , decodeBreadcrumbs
  , getEdgePortMapping
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
import           Empire.API.Data.TypeRep           (TypeRep(TLam, TStar, TCons))

import           Empire.ASTOp                      (ASTOp, match, runASTOp)
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Print               as Print
import qualified Empire.ASTOps.Read                as ASTRead
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import           Empire.Data.AST                   (NodeRef, astExceptionToException,
                                                    astExceptionFromException)
import           Empire.Data.Layers                (TypeLayer)
import           Empire.Empire

import qualified Luna.IR as IR
import qualified Luna.IR.Expr.Combinators as IR
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
            canEnter <- ASTRead.canEnterNode node
            when (not canEnter) $ throwM $ CannotEnterNodeException node
        _ -> return ()

buildGraph :: ASTOp m => m API.Graph
buildGraph = do
    throwIfCannotEnter
    connections <- buildConnections
    nodes <- buildNodes
    edges <- buildEdgeNodes connections
    let allNodes = nodes ++ case edges of
            Just (input, output) -> [input, output]
            _                    -> []
    API.Graph allNodes connections <$> buildMonads

buildNodes :: ASTOp m => m [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    nodes <- mapM buildNode allNodeIds
    return nodes

buildMonads :: ASTOp m => m [(TypeRep, [API.NodeId])]
buildMonads = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    let monad1 = (TCons "MonadMock1" [], List.sort allNodeIds) --FIXME[pm] provide real data
        monad2 = (TCons "MonadMock2" [], allNodeIds)
    return [monad1, monad2]

type EdgeNodes = (API.Node, API.Node)

buildEdgeNodes :: ASTOp m => [(OutPortRef, InPortRef)] -> m (Maybe EdgeNodes)
buildEdgeNodes connections = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge  <- buildInputEdge connections inputPort
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
            isLambda <- ASTRead.rhsIsLambda id'
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
    canEnter <- ASTRead.canEnterNode nid
    ports    <- buildPorts root
    let code    = Just $ Text.pack expr
        portMap = Map.fromList $ flip fmap ports $ \p@(Port id' _ _ _) -> (id', p)
    return $ API.Node nid name (API.ExpressionNode $ Text.pack expr) canEnter portMap (fromMaybe def meta) code

buildNodeTypecheckUpdate :: ASTOp m => NodeId -> m API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root   <- GraphUtils.getASTPointer nid
  match' <- ASTRead.isMatch root
  ref    <- if match' then GraphUtils.getASTTarget nid else return root
  ports  <- buildPorts ref
  let portMap = Map.fromList $ flip fmap ports $ \p@(Port id' _ _ _) -> (id', p)
  return $ API.NodeTypecheckerUpdate nid portMap

getNodeName :: ASTOp m => NodeId -> m (Maybe Text)
getNodeName nid = do
    root  <- GraphUtils.getASTPointer nid
    match' <- ASTRead.isMatch root
    if match' then do
        vnode <- GraphUtils.getASTVar nid
        name <- match vnode $ \case
            Var{}  -> ASTRead.getVarName vnode
            Cons{} -> Print.printNodeExpression vnode
        return $ Just (Text.pack name)
    else return Nothing

getPortState :: ASTOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then return Connected else match node $ \case
        IR.String s     -> return . WithDefault . Constant . StringValue $ s
        IR.Number i     -> return $ WithDefault $ Constant $ RationalValue 0 -- FIXME[MM]: put the number here
        Cons n _ -> do
            name <- pure $ nameToString n
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
            as   <- mapM Print.getTypeRep [node]
            return as
        _ -> return []

extractArgNames :: ASTOp m => NodeRef -> m [String]
extractArgNames node = do
    match node $ \case
        Lam{}    -> do
            args  <- ASTDeconstruct.extractArguments node
            names <- mapM ASTRead.getVarName args
            return names
        -- App is Lam that has some args applied
        App f _a -> extractArgNames =<< IR.source f
        Cons{}   -> do
            args  <- ASTDeconstruct.extractArguments node
            names <- mapM ASTRead.getVarName args
            return names
        _ -> return []

extractPortInfo :: ASTOp m => NodeRef -> m ([TypeRep], [PortState])
extractPortInfo node = do
    match node $ \case
        App f _args -> do
            unpacked   <- ASTDeconstruct.extractArguments node
            portStates <- mapM getPortState unpacked
            tp         <- do
                foo <- IR.readLayer @TypeLayer node
                IR.source foo
            types      <- extractArgTypes tp
            return (types, portStates)
        Lam _as o -> do
            args     <- ASTDeconstruct.extractArguments node
            areBlank <- mapM ASTRead.isBlank args
            isApp    <- ASTRead.isApp =<< IR.source o
            if and areBlank && isApp
                then do
                    extractPortInfo =<< IR.source o
                else do
                    tpRef <- IR.source =<< IR.readLayer @TypeLayer node
                    types <- extractArgTypes tpRef
                    return (types, [])
        Cons n _args -> do
            args       <- ASTDeconstruct.extractArguments node
            portStates <- mapM getPortState args
            types      <- IR.readLayer @TypeLayer node >>= IR.source >>= extractArgTypes
            return (types, portStates)
        _ -> do
            tpRef <- IR.source =<< IR.readLayer @TypeLayer node
            types <- extractArgTypes tpRef
            return (types, [])

there'sLambdaSomewhereThere :: ASTOp m => NodeRef -> m Bool
there'sLambdaSomewhereThere node = match node $ \case
    App f arg -> there'sLambdaSomewhereThere =<< IR.source f
    Lam{} -> return True
    _ -> return False

buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    (types, states) <- extractPortInfo ref
    names <- extractArgNames ref
    lambdaSomewhere <- there'sLambdaSomewhereThere ref
    isCons <- isJust <$> IR.narrowAtom @IR.Cons ref
    let additionalEmptyPort = if (not.null) types || lambdaSomewhere || isCons then 0
                              else if NotConnected `elem` states then 0 else 1
        portsTypes = types ++ replicate (max (length names) (length states) - length types + additionalEmptyPort) TStar
        namesGen = names ++ drop (length names) (("arg" ++) . show <$> [(0::Int)..])
        psCons = zipWith3 Port
                          (InPortId . Arg <$> [(0::Int)..])
                          namesGen
                          portsTypes
    return $ zipWith ($) psCons (states ++ repeat NotConnected)

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
    tp <- IR.source =<< IR.readLayer @TypeLayer ref
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

        flipPortId (OutPortId All)            = InPortId  (Arg 0)
        flipPortId (OutPortId (Projection i)) = InPortId  (Arg i)
        flipPortId (InPortId  (Arg i))        = OutPortId (Projection i)
        flipPortId (InPortId  Self)           = error "flipPortId: cannot flip self"

        flipPortName "Output" = "Input"
        flipPortName name     = name

buildConnections :: ASTOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- getEdgePortMapping
    connections <- mapM (getNodeInputs edges) allNodes
    outputEdgeConnections <- forM edges $ uncurry getOutputEdgeInputs
    let foo = maybeToList $ join outputEdgeConnections
    return $ foo ++ concat connections

buildInputEdge :: ASTOp m => [(OutPortRef, InPortRef)] -> NodeId -> m API.Node
buildInputEdge connections nid = do
    Just lastb <- use Graph.insideNode
    ref   <- GraphUtils.getASTTarget lastb
    (types, _states) <- extractPortInfo ref
    let connectedPorts = map (\(OutPortRef _ (Projection p)) -> p)
               $ map fst
               $ filter (\(OutPortRef refNid p,_) -> nid == refNid)
               $ connections
        states = map (\i -> if i `elem` connectedPorts then Connected else NotConnected) [(0::Int)..]
    names <- extractArgNames ref
    argTypes <- case types of
        [] -> do
            numberOfArguments <- length <$> (ASTDeconstruct.extractArguments ref)
            return $ replicate numberOfArguments TStar
        [TLam types' _] -> return types'
    let nameGen = names ++ drop (length names) (fmap (\i -> "arg" ++ show i) [(0::Int)..])
        inputEdges = List.zipWith4 (\n t state i -> Port (OutPortId $ Projection i) n t state) nameGen argTypes states [(0::Int)..]
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
        Lam _arg _body -> do
            out' <- ASTRead.getLambdaOutputRef lambda
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


resolveInputNodeId :: ASTOp m => Maybe (NodeId, NodeId) -> [NodeRef] -> NodeRef -> m (Maybe Int, Maybe NodeId)
resolveInputNodeId edgeNodes lambdaArgs ref = do
    nodeId <- ASTRead.getNodeId ref
    case List.findIndex (== ref) lambdaArgs of
        Just i -> return (Just i, fmap fst edgeNodes)
        _      -> return (Nothing, nodeId)

getOuterLambdaArguments :: ASTOp m => m [NodeRef]
getOuterLambdaArguments = do
    lambda <- use Graph.insideNode
    case lambda of
        Just lambda' -> do
            ref <- GraphUtils.getASTTarget lambda'
            lambdaArgs <- ASTDeconstruct.extractArguments ref
            return lambdaArgs
        _ -> return []

outIndexToProjection :: Maybe Int -> OutPort
outIndexToProjection Nothing = All
outIndexToProjection (Just i) = Projection i

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
                Just id' -> return [(OutPortRef id' Port.All, InPortRef nodeId (Port.Arg 0))]
                _        -> return []
        _       -> do
            selfMay     <- ASTRead.getSelfNodeRef ref
            lambdaArgs  <- getOuterLambdaArguments
            selfNodeMay <- case selfMay of
                Just self -> fmap snd $ resolveInputNodeId edgeNodes lambdaArgs self
                Nothing   -> return Nothing
            let projection  = case selfMay of
                    Just self -> Just $ outIndexToProjection $ List.findIndex (== self) lambdaArgs
                    Nothing   -> Nothing
            let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> projection)
                                  <*> (Just $ InPortRef nodeId Self)

            args     <- ASTDeconstruct.extractArguments ref
            nodeMays <- mapM (resolveInputNodeId edgeNodes lambdaArgs) args
            let withInd  = zipWith (\(outPortIndex, nodeId) index -> (outPortIndex, nodeId, index)) nodeMays [0..]
                hasNodeId (outIndex, Just nodeId, index) = Just (outIndex, nodeId, index)
                hasNodeId _ = Nothing
                onlyExt  = catMaybes $ map hasNodeId withInd
                conns    = flip map onlyExt $ \((outIndexToProjection -> proj), n, i) -> (OutPortRef n proj, InPortRef nodeId (Arg i))
            return $ maybeToList selfConnMay ++ conns
