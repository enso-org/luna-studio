{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Empire.Commands.GraphBuilder where

import           Control.Monad.State             hiding (when)
import           Data.Foldable                   (toList)
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Span                  (LeftSpacedSpan (..), SpacedSpan (..), leftSpacedSpan)
import           Empire.ASTOp                    (ClassOp, GraphOp, match, runASTOp)
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Print             as Print
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.GraphUtils      as GraphUtils
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (Graph)
import qualified Empire.Data.Graph               as Graph
import           Empire.Data.Layers              (Marker, SpanLength, TypeLayer)
import           Empire.Empire
import           Empire.Prelude                  hiding (toList, List)
import qualified Luna.IR                         as IR
import qualified Luna.IR.Term.Literal            as Lit
import           Luna.IR.Term.Uni
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem, Named (..))
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import qualified LunaStudio.Data.Graph           as API
import           LunaStudio.Data.LabeledTree     (LabeledTree (..))
import           LunaStudio.Data.MonadPath       (MonadPath (MonadPath))
import           LunaStudio.Data.Node            (NodeId)
import qualified LunaStudio.Data.Node            as API
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.Port            (InPort, InPortId, InPortIndex (..), InPortTree, InPorts (..), OutPort, OutPortId,
                                                  OutPortIndex (..), OutPortTree, OutPorts (..), Port (..), PortState (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..), _Constant)
import           LunaStudio.Data.PortRef         (InPortRef (..), OutPortRef (..), srcNodeId)
import           LunaStudio.Data.Position        (Position)
import           LunaStudio.Data.TypeRep         (TypeRep (TCons, TStar))
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Hardcoded as Parser (uminusName)
import qualified OCI.IR.Combinators              as IR

isDefinition :: BreadcrumbItem -> Bool
isDefinition def | Breadcrumb.Definition{} <- def = True
                 | otherwise                      = False

decodeBreadcrumbs :: Map.Map NodeId String -> Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs definitionsIDs bs@(Breadcrumb items) = runASTOp $ do
    bh    <- use Graph.breadcrumbHierarchy
    let funs = map (fmap Text.pack . flip Map.lookup definitionsIDs . view Breadcrumb.nodeId) $ takeWhile isDefinition items
        children = dropWhile isDefinition items
    names <- forM (BH.getBreadcrumbItems bh (Breadcrumb children)) $ \child -> getUniName $ child ^. BH.self
    pure $ Breadcrumb $ fmap (\(n, i) -> Named (fromMaybe "" n) i) $ zip (funs <> names) items

data CannotEnterNodeException = CannotEnterNodeException NodeId
    deriving Show
instance Exception CannotEnterNodeException where
    toException = astExceptionToException
    fromException = astExceptionFromException

buildGraph :: GraphOp m => m API.Graph
buildGraph = do
    connections <- buildConnections
    nodes       <- buildNodes
    (inE, outE) <- buildEdgeNodes
    API.Graph nodes connections (Just inE) (Just outE) <$> buildMonads

buildClassGraph :: ClassOp m => m API.Graph
buildClassGraph = do
    funs <- use Graph.clsFuns

    nodes' <- mapM (\(uuid, funGraph) -> buildClassNode uuid (funGraph ^. Graph.funName)) $ Map.assocs funs
    pure $ API.Graph nodes' [] Nothing Nothing []


buildClassNode :: ClassOp m => NodeId -> String -> m API.ExpressionNode
buildClassNode uuid name = do
    f    <- ASTRead.getFunByName name
    meta <- fromMaybe def <$> AST.readMeta f
    codeStart <- Code.functionBlockStartRef f
    LeftSpacedSpan (SpacedSpan _ len) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan f
    fileCode <- use Graph.code
    let code = Code.removeMarkers $ Text.take (fromIntegral len) $ Text.drop (fromIntegral codeStart) fileCode
    pure $ API.ExpressionNode uuid "" True (Just $ convert name) code (LabeledTree def (Port [] "base" TStar NotConnected)) (LabeledTree (OutPorts []) (Port [] "base" TStar NotConnected)) meta True

buildNodes :: GraphOp m => m [API.ExpressionNode]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNode allNodeIds
    pure nodes

buildMonads :: GraphOp m => m [MonadPath]
buildMonads = do
    allNodeIds <- getNodeIdSequence
    ioPath     <- filterM doesIO allNodeIds
    let ioMonad = MonadPath (TCons "IO" []) ioPath
    pure [ioMonad]

doesIO :: GraphOp m => NodeId -> m Bool
doesIO node = do
    ref <- ASTRead.getASTPointer node
    tp  <- IR.getLayer @TypeLayer ref >>= IR.source
    IR.matchExpr tp $ \case
        Monadic _ m -> hasIO =<< IR.source m
        _           -> pure False

hasIO :: GraphOp m => NodeRef -> m Bool
hasIO ref = IR.matchExpr ref $ \case
    Cons n _  -> pure $ n == "IO"
    Unify l r -> (||) <$> (hasIO =<< IR.source l) <*> (hasIO =<< IR.source r)
    _         -> pure False

getNodeIdSequence :: GraphOp m => m [NodeId]
getNodeIdSequence = do
    bodySeq <- ASTRead.getCurrentBody
    nodeSeq <- AST.readSeq bodySeq
    catMaybes <$> mapM getNodeIdWhenMarked nodeSeq

getNodeIdWhenMarked :: GraphOp m => NodeRef -> m (Maybe NodeId)
getNodeIdWhenMarked ref = match ref $ \case
    IR.Marked _m expr -> IR.source expr >>= ASTRead.getNodeId
    _                 -> pure Nothing

getMarkedExpr :: GraphOp m => NodeRef -> m NodeRef
getMarkedExpr ref = match ref $ \case
    IR.Marked _m expr -> IR.source expr
    _                 -> pure ref

type EdgeNodes = (API.InputSidebar, API.OutputSidebar)

buildEdgeNodes :: GraphOp m => m EdgeNodes
buildEdgeNodes = do
    (inputPort, outputPort) <- getEdgePortMapping
    inputEdge  <- buildInputSidebar  inputPort
    outputEdge <- buildOutputSidebar outputPort
    pure (inputEdge, outputEdge)

getEdgePortMapping :: (MonadIO m, GraphOp m) => m (NodeId, NodeId)
getEdgePortMapping = use $ Graph.breadcrumbHierarchy . BH.portMapping

aliasPortName :: Text
aliasPortName = "alias"

selfPortName :: Text
selfPortName = "self"

buildNodesForAutolayout :: GraphOp m => m [(NodeId, Int, Position)]
buildNodesForAutolayout = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNodeForAutolayout allNodeIds
    pure nodes

buildNodeForAutolayout :: GraphOp m => NodeId -> m (NodeId, Int, Position)
buildNodeForAutolayout nid = do
    marked       <- ASTRead.getASTRef nid
    Just codePos <- Code.getOffsetRelativeToFile marked
    meta         <- fromMaybe def <$> AST.readMeta marked
    pure (nid, fromIntegral codePos, meta ^. NodeMeta.position)

buildNode :: GraphOp m => NodeId -> m API.ExpressionNode
buildNode nid = do
    root      <- GraphUtils.getASTPointer nid
    ref       <- GraphUtils.getASTTarget  nid
    expr      <- Text.pack <$> Print.printExpression ref
    marked    <- ASTRead.getASTRef nid
    meta      <- fromMaybe def <$> AST.readMeta marked
    name      <- getNodeName nid
    canEnter  <- ASTRead.isEnterable ref
    inports   <- buildInPorts nid ref [] aliasPortName
    outports  <- buildOutPorts root
    code      <- Code.removeMarkers <$> getNodeCode nid
    pure $ API.ExpressionNode nid expr False name code inports outports meta canEnter

buildNodeTypecheckUpdate :: GraphOp m => NodeId -> m API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root     <- GraphUtils.getASTPointer nid
  ref      <- GraphUtils.getASTTarget  nid
  inPorts  <- buildInPorts nid ref [] aliasPortName
  outPorts <- buildOutPorts root
  pure $ API.ExpressionUpdate nid inPorts outPorts

getUniName :: GraphOp m => NodeRef -> m (Maybe Text)
getUniName root = do
    root'  <- getMarkedExpr root
    IR.matchExpr root' $ \case
        Unify       l _   -> Just . Text.pack <$> (Print.printName =<< IR.source l)
        ASGFunction n _ _ -> Just . Text.pack <$> (Print.printName =<< IR.source n)
        _ -> pure Nothing

getNodeName :: GraphOp m => NodeId -> m (Maybe Text)
getNodeName nid = ASTRead.getASTPointer nid >>= getUniName

getNodeCode :: GraphOp m => NodeId -> m Text
getNodeCode nid = do
    ref <- ASTRead.getASTTarget nid
    Code.getCodeOf ref

getDefault :: GraphOp m => NodeRef -> m (Maybe PortDefault)
getDefault arg = match arg $ \case
        IR.String s       -> pure $ Just $ Constant $ TextValue $ s
        IR.Number i       -> pure $ Just $ Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else RealValue $ Lit.toDouble i
        IR.Cons "True"  _ -> pure $ Just $ Constant $ BoolValue True
        IR.Cons "False" _ -> pure $ Just $ Constant $ BoolValue False
        IR.Blank          -> pure $ Nothing
        IR.Missing        -> pure $ Nothing
        _                 -> Just . Expression . Text.unpack <$> Print.printFullExpression arg

getInPortDefault :: GraphOp m => NodeRef -> Int -> m (Maybe PortDefault)
getInPortDefault ref pos = do
    args <- ASTDeconstruct.extractAppPorts ref
    let argRef = args ^? ix pos
    join <$> mapM getDefault argRef

getPortState :: GraphOp m => NodeRef -> m PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then pure Connected else match node $ \case
        IR.String s     -> pure . WithDefault . Constant . TextValue $ s
        IR.Number i     -> pure . WithDefault . Constant $ if Lit.isInteger i then IntValue $ Lit.toInt i else RealValue $ Lit.toDouble i
        Cons n _ -> do
            name <- pure $ nameToString n
            case name of
                "False" -> pure . WithDefault . Constant . BoolValue $ False
                "True"  -> pure . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression . Text.unpack <$> Print.printFullExpression node
        Blank   -> pure NotConnected
        Missing -> pure NotConnected
        App f a -> do
            negLit <- isNegativeLiteral node
            if negLit then do
                posLit <- getPortState =<< IR.source a
                let negate' (IntValue i) = IntValue (negate i)
                    negate' (RealValue v) = RealValue (negate v)
                let negated = posLit & Port._WithDefault . _Constant %~ negate'
                return negated
            else WithDefault . Expression . Text.unpack <$> Print.printFullExpression node
        _       -> WithDefault . Expression . Text.unpack <$> Print.printFullExpression node

extractArgTypes :: GraphOp m => NodeRef -> m [TypeRep]
extractArgTypes node = do
    match node $ \case
        Monadic s _ -> extractArgTypes =<< IR.source s
        Lam arg out -> (:) <$> (Print.getTypeRep =<< IR.source arg) <*> (extractArgTypes =<< IR.source out)
        _           -> pure []

safeGetVarName :: GraphOp m => NodeRef -> m (Maybe String)
safeGetVarName node = do
    name <- (Just <$> ASTRead.getVarName node) `catch`
        (\(e :: ASTRead.NoNameException) -> pure Nothing)
    pure name

extractArgNames :: GraphOp m => NodeRef -> m [Maybe String]
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
            pure $ map Just names
        ASGFunction _ a _ -> do
            args <- mapM IR.source a
            mapM safeGetVarName args
        _ -> pure []

extractAppArgNames :: GraphOp m => NodeRef -> m [Maybe String]
extractAppArgNames node = go [] node
    where
        go vars node = match node $ \case
            App f a -> do
                varName <- safeGetVarName =<< IR.source a
                go (varName : vars) =<< IR.source f
            Lam{}   -> extractArgNames node
            Cons{}  -> pure vars
            Var{}   -> pure vars
            Acc{}   -> pure vars
            _       -> pure []

insideThisNode :: GraphOp m => NodeRef -> m Bool
insideThisNode node = (== node) <$> ASTRead.getCurrentASTTarget

getPortsNames :: GraphOp m => NodeRef -> m [String]
getPortsNames node = do
    names <- extractArgNames node
    let backupNames = map (\i -> "arg" <> show i) [(0::Int)..]
    forM (zip names backupNames) $ \(name, backup) -> pure $ maybe backup id name

extractAppliedPorts :: GraphOp m => Bool -> Bool -> [NodeRef] -> NodeRef -> m [Maybe (TypeRep, PortState)]
extractAppliedPorts seenApp seenLam bound node = IR.matchExpr node $ \case
    Lam i o -> do
        inp   <- IR.source i
        nameH <- IR.matchExpr inp $ \case
            Var n -> pure $ Just $ unsafeHead $ convert n
            _     -> pure Nothing
        case (seenApp, nameH) of
            (_, Just '#') -> extractAppliedPorts seenApp seenLam (inp : bound) =<< IR.source o
            (False, _)    -> extractAppliedPorts False   True    (inp : bound) =<< IR.source o
            _          -> pure []
    App f a -> case seenLam of
        True  -> pure []
        False -> do
            arg          <- IR.source a
            isB          <- ASTRead.isBlank arg
            argTp        <- IR.getLayer @TypeLayer arg >>= IR.source
            res          <- if isB || elem arg bound then pure Nothing else Just .: (,) <$> Print.getTypeRep argTp <*> getPortState arg
            rest         <- extractAppliedPorts True False bound =<< IR.source f
            pure $ res : rest
    Tuple elts -> flip mapM elts $ \eltLink -> do
        elt   <- IR.source eltLink
        eltTp <- IR.getLayer @TypeLayer elt >>= IR.source
        Just .: (,) <$> Print.getTypeRep eltTp <*> getPortState elt
    _       -> pure []


fromMaybePort :: Maybe (TypeRep, PortState) -> (TypeRep, PortState)
fromMaybePort Nothing  = (TStar, NotConnected)
fromMaybePort (Just p) = p

mergePortInfo :: [Maybe (TypeRep, PortState)] -> [TypeRep] -> [(TypeRep, PortState)]
mergePortInfo []             []       = []
mergePortInfo (p : rest)     []       = fromMaybePort p : mergePortInfo rest []
mergePortInfo []             (t : ts) = (t, NotConnected) : mergePortInfo [] ts
mergePortInfo (Nothing : as) (t : ts) = (t, NotConnected) : mergePortInfo as ts
mergePortInfo (Just a  : as) ts       = a : mergePortInfo as ts

extractPortInfo :: GraphOp m => NodeRef -> m [(TypeRep, PortState)]
extractPortInfo n = do
    applied  <- reverse <$> extractAppliedPorts False False [] n
    tp       <- IR.getLayer @TypeLayer n >>= IR.source
    fromType <- extractArgTypes tp
    pure $ mergePortInfo applied fromType

isNegativeLiteral :: GraphOp m => NodeRef -> m Bool
isNegativeLiteral ref = match ref $ \case
    App f n -> do
        minus <- do
            IR.source f >>= (flip match $ \case
                Var n -> return $ n == Parser.uminusName
                _     -> return False)
        number <- do
            IR.source n >>= (flip match $ \case
                Number _ -> return True
                _        -> return False)
        return $ minus && number
    _ -> return False

buildArgPorts :: GraphOp m => InPortId -> NodeRef -> m [InPort]
buildArgPorts currentPort ref = do
    typed <- extractPortInfo ref
    names <- getPortsNames ref
    let portsTypes = fmap fst typed <> List.replicate (length names - length typed) TStar
        psCons = zipWith3 Port
                          ((currentPort <>) . pure . Arg <$> [(0::Int)..])
                          (map Text.pack $ names <> (("arg" <>) . show <$> [0..]))
                          portsTypes
    pure $ zipWith ($) psCons (fmap snd typed <> repeat NotConnected)

buildSelfPort :: GraphOp m => NodeId -> InPortId -> NodeRef -> m (Maybe (InPortTree InPort))
buildSelfPort nid currentPort node = do
    let potentialSelf = Port currentPort selfPortName TStar NotConnected
    match node $ \case
        Acc t _ -> do
            target <- IR.source t
            tree   <- buildInPorts nid target currentPort selfPortName
            pure $ Just tree
        Var _     -> pure $ Just $ LabeledTree def potentialSelf
        App f _   -> buildSelfPort nid currentPort =<< IR.source f
        Grouped g -> buildSelfPort nid currentPort =<< IR.source g
        _         -> pure Nothing

buildWholePort :: GraphOp m => NodeId -> InPortId -> Text -> NodeRef -> m InPort
buildWholePort nid currentPort portName ref = do
    tp    <- followTypeRep ref
    pid   <- ASTRead.getNodeId ref
    state <- if pid == Just nid then pure NotConnected else getPortState ref
    pure $ Port currentPort portName tp state

followTypeRep :: GraphOp m => NodeRef -> m TypeRep
followTypeRep ref = do
    tp <- IR.source =<< IR.getLayer @TypeLayer ref
    Print.getTypeRep tp

buildInPorts :: GraphOp m => NodeId -> NodeRef -> InPortId -> Text -> m (InPortTree InPort)
buildInPorts nid ref currentPort portName = do
    negLiteral <- isNegativeLiteral ref
    if negLiteral then do
        whole    <- buildWholePort nid currentPort portName ref
        pure $ LabeledTree (InPorts def def def) whole
    else do
        selfPort <- buildSelfPort nid (currentPort <> [Self]) ref
        argPorts <- buildArgPorts currentPort ref
        whole    <- buildWholePort nid currentPort portName ref
        pure $ LabeledTree (InPorts selfPort def (LabeledTree def <$> argPorts)) whole

buildDummyOutPort :: GraphOp m => NodeRef -> m (OutPortTree OutPort)
buildDummyOutPort ref = do
    tp <- followTypeRep ref
    pure $ LabeledTree (Port.OutPorts []) (Port [] "output" tp NotConnected)

buildOutPortTree :: GraphOp m => OutPortId -> NodeRef -> m (OutPortTree OutPort)
buildOutPortTree portId ref' = do
    ref   <- ASTRead.cutThroughGroups ref'
    name  <- Print.printName ref
    tp    <- followTypeRep ref
    let wholePort = Port portId (Text.pack name) tp NotConnected
    let buildSubtrees as = zipWithM buildOutPortTree ((portId <>) . pure . Port.Projection <$> [0 ..]) =<< mapM IR.source as
    children <- match ref $ \case
        Cons _ as -> buildSubtrees as
        Tuple as  -> buildSubtrees as
        List  as  -> buildSubtrees as
        _         -> pure []
    pure $ LabeledTree (OutPorts children) wholePort

buildOutPorts :: GraphOp m => NodeRef -> m (OutPortTree OutPort)
buildOutPorts ref = match ref $ \case
    Unify l r -> buildOutPortTree [] =<< IR.source l
    _         -> buildDummyOutPort ref


buildConnections :: GraphOp m => m [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes       <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    (_, outEdge)   <- getEdgePortMapping
    connections    <- mapM getNodeInputs allNodes
    outputEdgeConn <- getOutputSidebarInputs outEdge
    pure $ (maybeToList outputEdgeConn) <> concat connections

buildInputSidebarTypecheckUpdate :: GraphOp m => NodeId -> m API.NodeTypecheckerUpdate
buildInputSidebarTypecheckUpdate nid = do
    API.InputSidebar nid ps _ <- buildInputSidebar nid
    pure $ API.InputSidebarUpdate nid ps


buildInputSidebar :: GraphOp m => NodeId -> m API.InputSidebar
buildInputSidebar nid = do
    ref      <- ASTRead.getCurrentASTTarget
    isDef    <- ASTRead.isASGFunction ref
    args     <- ASTDeconstruct.extractFunctionPorts ref
    argTrees <- zipWithM buildOutPortTree (pure . Projection <$> [0..]) args
    pure $ API.InputSidebar nid argTrees isDef

buildOutputSidebarTypecheckUpdate :: GraphOp m => NodeId -> m API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid m <- buildOutputSidebar nid
    pure $ API.OutputSidebarUpdate nid m

buildOutputSidebar :: GraphOp m => NodeId -> m API.OutputSidebar
buildOutputSidebar nid = do
    ref   <- ASTRead.getCurrentASTTarget
    out   <- ASTRead.getLambdaOutputRef ref
    tp    <- followTypeRep out
    state <- getPortState  out
    pure $ API.OutputSidebar nid $ LabeledTree (Port.InPorts Nothing Nothing [])  $ Port [] "output" tp state

getOutputSidebarInputs :: GraphOp m => NodeId -> m (Maybe (OutPortRef, InPortRef))
getOutputSidebarInputs outputEdge = do
    ref     <- ASTRead.getCurrentASTTarget
    out     <- ASTRead.getLambdaOutputRef ref
    wholeIn <- resolveInput out
    pure $ (, InPortRef (NodeLoc def outputEdge) []) <$> wholeIn

nodeConnectedToOutput :: GraphOp m => m (Maybe NodeId)
nodeConnectedToOutput = do
    edges  <- fmap Just $ use $ Graph.breadcrumbHierarchy . BH.portMapping
    fmap join $ forM edges $ \(i, o) -> do
        connection <- getOutputSidebarInputs o
        pure $ (view $ _1 . srcNodeId) <$> connection

resolveInput :: GraphOp m => NodeRef -> m (Maybe OutPortRef)
resolveInput = IR.getLayer @Marker

deepResolveInputs :: GraphOp m => NodeId -> NodeRef -> InPortRef -> m [(OutPortRef, InPortRef)]
deepResolveInputs nid ref portRef@(InPortRef loc id) = do
    currentPortResolution <- toList <$> resolveInput ref
    let currentPortConn    = (, portRef) <$> (filter ((/= nid) . view srcNodeId) currentPortResolution)
        unfilteredPortConn = (, portRef) <$> currentPortResolution
    args       <- ASTDeconstruct.extractAppPorts ref
    argsConns  <- forM (zip args [0..]) $ \(arg, i) -> deepResolveInputs nid arg (InPortRef loc (id <> [Arg i]))
    head       <- ASTDeconstruct.extractFun ref
    self       <- ASTDeconstruct.extractSelf head
    firstRun   <- (== ref) <$> ASTRead.getASTTarget nid
    headConns <- case (self, head == ref) of
        (Just s, _) -> deepResolveInputs nid s    (InPortRef loc (id <> [Self]))
        (_, False)  -> deepResolveInputs nid head (InPortRef loc (id <> [Head]))
        (_, True)   -> pure $ if null currentPortConn && not firstRun then unfilteredPortConn else []
        _           -> pure []
    pure $ concat [currentPortConn, headConns, concat argsConns]

getNodeInputs :: GraphOp m => NodeId -> m [(OutPortRef, InPortRef)]
getNodeInputs nid = do
    let loc = NodeLoc def nid
    ref      <- ASTRead.getASTTarget   nid
    deepResolveInputs nid ref (InPortRef loc [])
