module Empire.Commands.GraphBuilder where

import           Control.Lens                    (uses)
import           Control.Monad.State             hiding (when)
import           Data.Foldable                   (toList)
import           Data.Char                       (intToDigit)
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, maybeToList)
import qualified Data.Mutable.Class              as Mutable
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Span                  (SpacedSpan (..), leftSpacedSpan)
import qualified Data.Vector.Storable.Foreign    as Vector
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
import           Empire.Prelude                  hiding (read, toList)
import qualified Luna.IR                         as IR
import qualified Luna.IR.Term.Literal            as Lit
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem, Named (..))
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import qualified LunaStudio.Data.Connection      as API
import qualified LunaStudio.Data.Graph           as API
import           LunaStudio.Data.LabeledTree     (LabeledTree (..))
import           LunaStudio.Data.MonadPath       (MonadPath (MonadPath))
import           LunaStudio.Data.NodeId          (NodeId)
import qualified LunaStudio.Data.Node            as API
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.Port            (InPort, InPortId, InPortIndex (..), InPortTree, InPorts (..), OutPort, OutPortId,
                                                  OutPortIndex (..), OutPortTree, OutPorts (..), Port (..), PortState (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..), _Constant)
import qualified LunaStudio.Data.PortRef         as PortRef
import           LunaStudio.Data.PortRef         (InPortRef (..), OutPortRef (..), srcNodeId, srcNodeLoc)
import           LunaStudio.Data.Position        (Position)
import           LunaStudio.Data.TypeRep         (TypeRep (TCons, TStar))
import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Parser (uminus)
-- import qualified OCI.IR.Combinators              as IR
import           Prelude (read)

isDefinition :: BreadcrumbItem -> Bool
isDefinition def | Breadcrumb.Definition{} <- def = True
                 | otherwise                      = False

decodeBreadcrumbs :: Map.Map NodeId String -> Breadcrumb BreadcrumbItem
    -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs definitionsIDs bs@(Breadcrumb items) = runASTOp $ do
    bh    <- use Graph.breadcrumbHierarchy
    let toFun = fmap Text.pack
            . flip Map.lookup definitionsIDs
            . view Breadcrumb.nodeId
        funs = toFun <$> takeWhile isDefinition items
        children = dropWhile isDefinition items
    names <- forM (BH.getBreadcrumbItems bh (Breadcrumb children))
        $ \child -> getUniName $ child ^. BH.self
    pure $ Breadcrumb $
        (\(n, i) -> Named (fromMaybe "" n) i) <$> zip (funs <> names) items

data CannotEnterNodeException = CannotEnterNodeException NodeId
    deriving Show
instance Exception CannotEnterNodeException where
    toException = astExceptionToException
    fromException = astExceptionFromException

buildGraph :: GraphOp API.Graph
buildGraph = do
    connections <- buildConnections
    nodes       <- buildNodes
    (inE, outE) <- buildEdgeNodes
    API.Graph
        nodes
        (uncurry API.Connection <$> connections)
        (Just inE)
        (Just outE)
        <$> buildMonads

buildClassGraph :: ClassOp API.Graph
buildClassGraph = do
    funs <- use Graph.clsFuns
    nodes' <- mapM (\(uuid, funGraph)
        -> buildClassNode uuid (funGraph ^. Graph.funName)) $ Map.assocs funs
    pure $ API.Graph nodes' mempty mempty mempty mempty


buildClassNode :: NodeId -> String -> ClassOp API.ExpressionNode
buildClassNode uuid name = do
    f    <- ASTRead.getFunByNodeId uuid
    meta <- fromMaybe def <$> AST.readMeta f
    codeStart <- Code.functionBlockStartRef f
    LeftSpacedSpan (SpacedSpan _ len)
        <- view CodeSpan.realSpan <$> getLayer @CodeSpan f
    fileCode <- use Graph.code
    let code = Code.removeMarkers $ Text.take (fromIntegral len)
            $ Text.drop (fromIntegral codeStart) fileCode
    pure $ API.ExpressionNode
            uuid
            ""
            True
            (Just $ convert name)
            code
            (LabeledTree def (Port [] "base" TStar NotConnected))
            (LabeledTree (OutPorts []) (Port [] "base" TStar NotConnected))
            meta
            True

buildNodes :: GraphOp [API.ExpressionNode]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNode allNodeIds
    pure nodes

buildMonads :: GraphOp [MonadPath]
buildMonads = do
    allNodeIds <- getNodeIdSequence
    ioPath     <- filterM doesIO allNodeIds
    let ioMonad = MonadPath (TCons "IO" []) ioPath
    pure [ioMonad]

doesIO :: NodeId -> GraphOp Bool
doesIO node = do
    ref <- ASTRead.getASTPointer node
    tp  <- getLayer @TypeLayer ref >>= source
    matchExpr tp $ \case
        -- Monadic _ m -> hasIO =<< source m
        _           -> pure False

hasIO :: NodeRef -> GraphOp Bool
hasIO ref = matchExpr ref $ \case
    Cons n _  -> pure $ n == "IO"
    Unify l r -> (||) <$> (hasIO =<< source l) <*> (hasIO =<< source r)
    _         -> pure False

getNodeIdSequence :: GraphOp [NodeId]
getNodeIdSequence = do
    bodySeq <- ASTRead.getCurrentBody
    nodeSeq <- AST.readSeq bodySeq
    catMaybes <$> mapM getNodeIdWhenMarked nodeSeq

getNodeIdWhenMarked :: NodeRef -> GraphOp (Maybe NodeId)
getNodeIdWhenMarked ref = match ref $ \case
    Marked _m expr -> source expr >>= ASTRead.getNodeId
    _              -> pure Nothing

getMarkedExpr :: NodeRef -> GraphOp NodeRef
getMarkedExpr ref = match ref $ \case
    Marked _m expr -> source expr
    _              -> pure ref

type EdgeNodes = (API.InputSidebar, API.OutputSidebar)

buildEdgeNodes :: GraphOp EdgeNodes
buildEdgeNodes = do
    (inputPort, outputPort) <- getEdgePortMapping
    inputEdge  <- buildInputSidebar  inputPort
    outputEdge <- buildOutputSidebar outputPort
    pure (inputEdge, outputEdge)

getEdgePortMapping :: GraphOp (NodeId, NodeId)
getEdgePortMapping = use $ Graph.breadcrumbHierarchy . BH.portMapping

aliasPortName :: Text
aliasPortName = "alias"

selfPortName :: Text
selfPortName = "self"

buildNodesForAutolayout :: GraphOp [(NodeId, Int, Position)]
buildNodesForAutolayout = do
    allNodeIds <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    nodes      <- mapM buildNodeForAutolayout allNodeIds
    pure nodes

buildNodeForAutolayout :: NodeId -> GraphOp (NodeId, Int, Position)
buildNodeForAutolayout nid = do
    marked       <- ASTRead.getASTRef nid
    Just codePos <- Code.getOffsetRelativeToFile marked
    meta         <- fromMaybe def <$> AST.readMeta marked
    pure (nid, fromIntegral codePos, meta ^. NodeMeta.position)

buildNodesForAutolayoutCls :: ClassOp [(NodeId, Int, Position)]
buildNodesForAutolayoutCls = do
    allNodeIds <- uses Graph.clsFuns Map.keys
    nodes      <- mapM buildNodeForAutolayoutCls allNodeIds
    pure nodes

buildNodeForAutolayoutCls :: NodeId -> ClassOp (NodeId, Int, Position)
buildNodeForAutolayoutCls nid = do
    name    <- use $ Graph.clsFuns . ix nid . Graph.funName
    marked  <- ASTRead.getFunByNodeId nid
    codePos <- Code.functionBlockStartRef marked
    meta    <- fromMaybe def <$> AST.readMeta marked
    pure (nid, fromIntegral codePos, meta ^. NodeMeta.position)

buildNode :: NodeId -> GraphOp API.ExpressionNode
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
    pure $ API.ExpressionNode
        nid expr False name code inports outports meta canEnter

buildNodeTypecheckUpdate :: NodeId -> GraphOp API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate nid = do
  root     <- GraphUtils.getASTPointer nid
  ref      <- GraphUtils.getASTTarget  nid
  inPorts  <- buildInPorts nid ref [] aliasPortName
  outPorts <- buildOutPorts root
  pure $ API.ExpressionUpdate nid inPorts outPorts

getUniName :: NodeRef -> GraphOp (Maybe Text)
getUniName root = do
    root'  <- getMarkedExpr root
    matchExpr root' $ \case
        Unify       l _   -> Just . Text.pack <$> (Print.printName =<< source l)
        ASGFunction n _ _ -> Just . Text.pack <$> (Print.printName =<< source n)
        _ -> pure Nothing

getNodeName :: NodeId -> GraphOp (Maybe Text)
getNodeName nid = ASTRead.getASTPointer nid >>= getUniName

getNodeCode :: NodeId -> GraphOp Text
getNodeCode nid = do
    ref <- ASTRead.getASTTarget nid
    Code.getCodeOf ref

getDefault :: NodeRef -> GraphOp (Maybe PortDefault)
getDefault arg = match arg $ \case
        IRString s       -> Just . Constant . TextValue <$> Mutable.toList s
        IRNumber b i f   -> do
            fracPart <- map (intToDigit . fromIntegral) <$> Mutable.toList f
            intPart  <- map (intToDigit . fromIntegral) <$> Mutable.toList i
            pure $ Just $ Constant $ if fracPart == "" then IntValue $ read intPart else RealValue $ read intPart + read fracPart
        Cons "True"  _ -> pure $ Just $ Constant $ BoolValue True
        Cons "False" _ -> pure $ Just $ Constant $ BoolValue False
        Blank          -> pure $ Nothing
        Missing        -> pure $ Nothing
        _                 -> Just . Expression . Text.unpack <$> Print.printFullExpression arg

getInPortDefault :: NodeRef -> Int -> GraphOp (Maybe PortDefault)
getInPortDefault ref pos = do
    args <- ASTDeconstruct.extractAppPorts ref
    let argRef = args ^? ix pos
    join <$> mapM getDefault argRef

getPortState :: NodeRef -> GraphOp PortState
getPortState node = do
    isConnected <- ASTRead.isGraphNode node
    if isConnected then pure Connected else match node $ \case
        IRString s     -> WithDefault . Constant . TextValue <$> Mutable.toList s
        IRNumber b i f   -> do
            fracPart <- map (intToDigit . fromIntegral) <$> Mutable.toList f
            intPart  <- map (intToDigit . fromIntegral) <$> Mutable.toList i
            pure $ WithDefault $ Constant $ if fracPart == "" then IntValue $ read intPart else RealValue $ read intPart + read fracPart
        Cons n _ -> do
            name <- pure $ nameToString n
            case name of
                "False" -> pure . WithDefault . Constant . BoolValue $ False
                "True"  -> pure . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression . Text.unpack
                    <$> Print.printFullExpression node
        Blank   -> pure NotConnected
        Missing -> pure NotConnected
        App f a -> do
            negLit <- isNegativeLiteral node
            if negLit then do
                posLit <- getPortState =<< source a
                let negate' (IntValue i) = IntValue (negate i)
                    negate' (RealValue v) = RealValue (negate v)
                let negated = posLit & Port._WithDefault . _Constant %~ negate'
                return negated
            else WithDefault . Expression . Text.unpack 
                <$> Print.printFullExpression node
        _ -> WithDefault . Expression . Text.unpack
            <$> Print.printFullExpression node

extractArgTypes :: NodeRef -> GraphOp [TypeRep]
extractArgTypes node = do
    match node $ \case
        -- Monadic s _ -> extractArgTypes =<< source s
        Lam arg out -> (:) <$> (Print.getTypeRep =<< source arg) <*> (extractArgTypes =<< source out)
        _           -> pure []

safeGetVarName :: NodeRef -> GraphOp (Maybe String)
safeGetVarName node = do
    name <- (Just <$> ASTRead.getVarName node) `catch`
        (\(e :: ASTRead.NoNameException) -> pure Nothing)
    pure name

extractArgNames :: NodeRef -> GraphOp [Maybe String]
extractArgNames node = do
    match node $ \case
        Grouped g -> source g >>= extractArgNames
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
            args <- mapM source =<< ptrListToList a
            mapM safeGetVarName args
        _ -> pure []

extractAppArgNames :: NodeRef -> GraphOp [Maybe String]
extractAppArgNames node = go [] node
    where
        go :: [Maybe String] -> NodeRef -> GraphOp [Maybe String]
        go vars node = match node $ \case
            App f a -> do
                varName <- safeGetVarName =<< source a
                go (varName : vars) =<< source f
            Lam{}   -> extractArgNames node
            Cons{}  -> pure vars
            Var{}   -> pure vars
            Acc{}   -> pure vars
            _       -> pure []

insideThisNode :: NodeRef -> GraphOp Bool
insideThisNode node = (== node) <$> ASTRead.getCurrentASTTarget

getPortsNames :: NodeRef -> GraphOp [String]
getPortsNames node = do
    names <- extractArgNames node
    let backupNames = map (\i -> "arg" <> show i) [(0::Int)..]
    forM (zip names backupNames)
        $ \(name, backup) -> pure $ maybe backup id name

extractAppliedPorts ::
    Bool -> Bool -> [NodeRef] -> NodeRef -> GraphOp [Maybe (TypeRep, PortState)]
extractAppliedPorts seenApp seenLam bound node = matchExpr node $ \case
    Lam i o -> do
        inp   <- source i
        nameH <- matchExpr inp $ \case
            Var n -> pure $ Just $ unsafeHead $ convert n
            _     -> pure Nothing
        case (seenApp, nameH) of
            (_, Just '#') -> extractAppliedPorts seenApp seenLam (inp : bound) =<< source o
            (False, _)    -> extractAppliedPorts False   True    (inp : bound) =<< source o
            _          -> pure []
    App f a -> case seenLam of
        True  -> pure []
        False -> do
            arg          <- source a
            isB          <- ASTRead.isBlank arg
            argTp        <- getLayer @TypeLayer arg >>= source
            res          <- if isB || elem arg bound
                then pure Nothing
                else Just .: (,) <$> Print.getTypeRep argTp <*> getPortState arg
            rest         <- extractAppliedPorts True False bound =<< source f
            pure $ res : rest
    Tuple elts' -> do
        elts <- ptrListToList elts'
        forM elts $ \eltLink -> do
            elt   <- source eltLink
            eltTp <- getLayer @TypeLayer elt >>= source
            Just .: (,) <$> Print.getTypeRep eltTp <*> getPortState elt
    _       -> pure []


fromMaybePort :: Maybe (TypeRep, PortState) -> (TypeRep, PortState)
fromMaybePort Nothing  = (TStar, NotConnected)
fromMaybePort (Just p) = p

mergePortInfo :: [Maybe (TypeRep, PortState)] -> [TypeRep]
    -> [(TypeRep, PortState)]
mergePortInfo []             []       = []
mergePortInfo (p : rest)     []       = fromMaybePort p : mergePortInfo rest []
mergePortInfo []             (t : ts) = (t, NotConnected) : mergePortInfo [] ts
mergePortInfo (Nothing : as) (t : ts) = (t, NotConnected) : mergePortInfo as ts
mergePortInfo (Just a  : as) ts       = a : mergePortInfo as ts

extractPortInfo :: NodeRef -> GraphOp [(TypeRep, PortState)]
extractPortInfo n = do
    applied  <- reverse <$> extractAppliedPorts False False [] n
    tp       <- getLayer @TypeLayer n >>= source
    fromType <- extractArgTypes tp
    pure $ mergePortInfo applied fromType

isNegativeLiteral :: NodeRef -> GraphOp Bool
isNegativeLiteral ref = match ref $ \case
    App f n -> do
        minus <- do
            source f >>= (flip match $ \case
                Var n -> return $ n == Parser.uminus
                _     -> return False)
        number <- do
            source n >>= (flip match $ \case
                IRNumber{} -> return True
                _        -> return False)
        return $ minus && number
    _ -> return False

buildArgPorts :: InPortId -> NodeRef -> GraphOp [InPort]
buildArgPorts currentPort ref = do
    typed <- extractPortInfo ref
    names <- getPortsNames ref
    let portsTypes = fmap fst typed
            <> List.replicate (length names - length typed) TStar
        psCons = zipWith3 Port
            ((currentPort <>) . pure . Arg <$> [(0::Int)..])
            (map Text.pack $ names <> (("arg" <>) . show <$> [0..]))
            portsTypes
    pure $ zipWith ($) psCons (fmap snd typed <> repeat NotConnected)

buildSelfPort ::
    NodeId -> InPortId -> NodeRef -> GraphOp (Maybe (InPortTree InPort))
buildSelfPort nid currentPort node = do
    let potentialSelf = Port currentPort selfPortName TStar NotConnected
    match node $ \case
        Acc t _ -> do
            target <- source t
            tree   <- buildInPorts nid target currentPort selfPortName
            pure $ Just tree
        Var _     -> pure $ Just $ LabeledTree def potentialSelf
        App f _   -> buildSelfPort nid currentPort =<< source f
        Grouped g -> buildSelfPort nid currentPort =<< source g
        _         -> pure Nothing

buildWholePort :: NodeId -> InPortId -> Text -> NodeRef -> GraphOp InPort
buildWholePort nid currentPort portName ref = do
    tp    <- followTypeRep ref
    pid   <- ASTRead.getNodeId ref
    state <- if pid == Just nid then pure NotConnected else getPortState ref
    pure $ Port currentPort portName tp state

followTypeRep :: NodeRef -> GraphOp TypeRep
followTypeRep ref = do
    tp <- source =<< getLayer @TypeLayer ref
    Print.getTypeRep tp

buildInPorts ::
    NodeId -> NodeRef -> InPortId -> Text -> GraphOp (InPortTree InPort)
buildInPorts nid ref currentPort portName = do
    negLiteral <- isNegativeLiteral ref
    if negLiteral then do
        whole    <- buildWholePort nid currentPort portName ref
        pure $ LabeledTree (InPorts def def def) whole
    else do
        selfPort <- buildSelfPort nid (currentPort <> [Self]) ref
        argPorts <- buildArgPorts currentPort ref
        whole    <- buildWholePort nid currentPort portName ref
        pure $ LabeledTree
            (InPorts selfPort def (LabeledTree def <$> argPorts))
            whole

buildDummyOutPort :: NodeRef -> GraphOp (OutPortTree OutPort)
buildDummyOutPort ref = do
    tp <- followTypeRep ref
    pure $ LabeledTree (Port.OutPorts []) (Port [] "output" tp NotConnected)

buildOutPortTree :: OutPortId -> NodeRef -> GraphOp (OutPortTree OutPort)
buildOutPortTree portId ref' = do
    ref   <- ASTRead.cutThroughGroups ref'
    name  <- Print.printName ref
    tp    <- followTypeRep ref
    let wholePort = Port portId (Text.pack name) tp NotConnected
    let buildSubtrees as = zipWithM
            buildOutPortTree
            ((portId <>) . pure . Port.Projection <$> [0 ..])
            =<< mapM source as
    children <- match ref $ \case
        Cons _ as -> buildSubtrees . coerce =<< ptrListToList as
        Tuple as  -> buildSubtrees . coerce =<< ptrListToList as
        List  as  -> buildSubtrees . coerce =<< ptrListToList as
        _         -> pure []
    pure $ LabeledTree (OutPorts children) wholePort

buildOutPorts :: NodeRef -> GraphOp (OutPortTree OutPort)
buildOutPorts ref = match ref $ \case
    Unify l r -> buildOutPortTree [] =<< source l
    _         -> buildDummyOutPort ref


buildConnections :: GraphOp [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes       <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
    (_, outEdge)   <- getEdgePortMapping
    connections    <- mapM getNodeInputs allNodes
    outputEdgeConn <- getOutputSidebarInputs outEdge
    pure $ (maybeToList outputEdgeConn) <> concat connections

buildInputSidebarTypecheckUpdate ::
    NodeId -> GraphOp API.NodeTypecheckerUpdate
buildInputSidebarTypecheckUpdate nid = do
    API.InputSidebar nid ps _ <- buildInputSidebar nid
    pure $ API.InputSidebarUpdate nid ps


buildInputSidebar :: NodeId -> GraphOp API.InputSidebar
buildInputSidebar nid = do
    ref      <- ASTRead.getCurrentASTTarget
    isDef    <- ASTRead.isASGFunction ref
    args     <- ASTDeconstruct.extractFunctionPorts ref
    argTrees <- zipWithM buildOutPortTree (pure . Projection <$> [0..]) args
    pure $ API.InputSidebar nid argTrees isDef

buildOutputSidebarTypecheckUpdate :: NodeId -> GraphOp API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid m <- buildOutputSidebar nid
    pure $ API.OutputSidebarUpdate nid m

buildOutputSidebar :: NodeId -> GraphOp API.OutputSidebar
buildOutputSidebar nid = do
    ref   <- ASTRead.getCurrentASTTarget
    out   <- ASTRead.getLambdaOutputRef ref
    tp    <- followTypeRep out
    state <- getPortState  out
    pure $ API.OutputSidebar nid $ LabeledTree (Port.InPorts Nothing Nothing [])
        $ Port [] "output" tp state

getOutputSidebarInputs :: NodeId -> GraphOp (Maybe (OutPortRef, InPortRef))
getOutputSidebarInputs outputEdge = do
    ref     <- ASTRead.getCurrentASTTarget
    out     <- ASTRead.getLambdaOutputRef ref
    wholeIn <- resolveInput out
    pure $ (, InPortRef (NodeLoc def outputEdge) []) <$> wholeIn

nodeConnectedToOutput :: GraphOp (Maybe NodeId)
nodeConnectedToOutput = do
    edges  <- fmap Just $ use $ Graph.breadcrumbHierarchy . BH.portMapping
    fmap join $ forM edges $ \(i, o) -> do
        connection <- getOutputSidebarInputs o
        let a = (view srcNodeId . fst) <$> connection
        return a

resolveInput :: NodeRef -> GraphOp (Maybe OutPortRef)
resolveInput n = traverse fromPortMarker =<< getLayer @Marker n

deepResolveInputs ::
    NodeId -> NodeRef -> InPortRef -> GraphOp [(OutPortRef, InPortRef)]
deepResolveInputs nid ref portRef@(InPortRef loc id) = do
    currentPortResolution <- toList <$> resolveInput ref
    let currentPortConn    = (, portRef)
            <$> (filter ((/= nid) . view srcNodeId) currentPortResolution)
        unfilteredPortConn = (, portRef) <$> currentPortResolution
    args       <- ASTDeconstruct.extractAppPorts ref
    argsConns  <- forM (zip args [0..]) $ \(arg, i)
        -> deepResolveInputs nid arg (InPortRef loc (id <> [Arg i]))
    head       <- ASTDeconstruct.extractFun ref
    self       <- ASTDeconstruct.extractSelf head
    firstRun   <- (== ref) <$> ASTRead.getASTTarget nid
    headConns <- case (self, head == ref) of
        (Just s, _) -> deepResolveInputs nid s    (InPortRef loc (id <> [Self]))
        (_, False)  -> deepResolveInputs nid head (InPortRef loc (id <> [Head]))
        (_, True)   -> pure $ if null currentPortConn && not firstRun
            then unfilteredPortConn
            else []
        _           -> pure []
    pure $ concat [currentPortConn, headConns, concat argsConns]

getNodeInputs :: NodeId -> GraphOp [(OutPortRef, InPortRef)]
getNodeInputs nid = do
    let loc = NodeLoc def nid
    ref      <- ASTRead.getASTTarget   nid
    deepResolveInputs nid ref (InPortRef loc [])
