{-# LANGUAGE PatternSynonyms #-}

module Empire.Commands.GraphBuilder where

import Empire.Prelude hiding (mod, fromType, read, toList)
import Prelude        (read)

import qualified Data.List                            as List
import qualified Data.Map                             as Map
import qualified Data.Mutable.Class                   as Mutable
import qualified Data.Set                             as Set
import qualified Data.Text                            as Text
import qualified Empire.ASTOps.Deconstruct            as ASTDeconstruct
import qualified Empire.ASTOps.Print                  as Print
import qualified Empire.ASTOps.Read                   as ASTRead
import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Code                 as Code
import qualified Empire.Commands.GraphUtils           as GraphUtils
import qualified Empire.Data.BreadcrumbHierarchy      as BH
import qualified Empire.Data.Graph                    as Graph
import qualified Luna.IR                              as IR
import qualified Luna.Pass.Typing.Data.Target         as Target
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Lexer.Names  as Parser (uminus)
import qualified LunaStudio.Data.Breadcrumb           as Breadcrumb
import qualified LunaStudio.Data.Connection           as API
import qualified LunaStudio.Data.Graph                as API
import qualified LunaStudio.Data.Node                 as API
import qualified LunaStudio.Data.NodeMeta             as NodeMeta
import qualified LunaStudio.Data.Port                 as Port
import qualified Safe

import Control.Lens                         (has, uses)
import Control.Monad.State                  hiding (state, void, when)
import Data.Char                            (intToDigit)
import Data.Foldable                        (toList)
import Data.Maybe                           (catMaybes, maybeToList)
import Data.Text                            (Text)
import Data.Text.Span                       (SpacedSpan (SpacedSpan))
import Empire.ASTOp                         (ClassOp, GraphOp, match, runASTOp)
import Empire.Data.AST                      (NodeRef, astExceptionFromException,
                                             astExceptionToException)
import Empire.Data.Graph                    (Graph)
import Empire.Data.Layers                   (Marker, TypeLayer)
import Empire.Empire                        (Command)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb),
                                             BreadcrumbItem, Named (Named))
import LunaStudio.Data.LabeledTree          (LabeledTree (LabeledTree))
import LunaStudio.Data.MonadPath            (MonadPath (MonadPath))
import LunaStudio.Data.NodeId               (NodeId)
import LunaStudio.Data.NodeLoc              (NodeLoc (NodeLoc))
import LunaStudio.Data.Port                 (InPort, InPortId,
                                             InPortIndex (Arg, Head, Self),
                                             InPortTree, InPorts (InPorts),
                                             OutPort, OutPortId,
                                             OutPortIndex (Projection),
                                             OutPortTree, OutPorts (OutPorts),
                                             Port (Port),
                                             PortState (Connected, NotConnected, WithDefault))
import LunaStudio.Data.PortDefault          (PortDefault (Constant, Expression), PortValue (BoolValue, IntValue, RealValue, TextValue),
                                             _Constant)
import LunaStudio.Data.PortRef              (InPortRef (InPortRef), OutPortRef,
                                             srcNodeId, dstPortId)
import LunaStudio.Data.Position             (Position)
import LunaStudio.Data.TypeRep              (TypeRep (TCons, TStar))


isDefinition :: BreadcrumbItem -> Bool
isDefinition = has Breadcrumb._Definition

decodeBreadcrumbs :: Map.Map NodeId String -> Breadcrumb BreadcrumbItem
    -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs definitionsIDs (Breadcrumb items) = runASTOp $ do
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
    monads      <- buildMonads
    pure $ API.Graph
        nodes
        (uncurry API.Connection <$> connections)
        (Just inE)
        (Just outE)
        monads
        mempty

buildClassGraph :: ClassOp API.Graph
buildClassGraph = do
    funs <- use Graph.clsFuns
    nodes' <- mapM (\(uuid, funGraph)
        -> buildClassNode uuid (funGraph ^. Graph.funName)) $ Map.assocs funs
    pure $ API.Graph nodes' mempty mempty mempty mempty mempty


buildClassNode :: NodeId -> String -> ClassOp API.ExpressionNode
buildClassNode uuid name = do
    f    <- ASTRead.getFunByNodeId uuid
    codeStart <- Code.functionBlockStartRef f
    (nameOff, nameLen) <- ASTRead.cutThroughDocAndMarked f >>= \a -> matchExpr a $ \case
        ASGFunction n _ _ -> do
            LeftSpacedSpan (SpacedSpan off nlen) <- Code.getOffset =<< source n
            LeftSpacedSpan (SpacedSpan _ len)
                <- view CodeSpan.realSpan <$> (getLayer @CodeSpan =<< source n)
            return (off + nlen, off + fromIntegral len)
        uni -> error ("unexpected constructor as function: " <> show uni <> " " <> name)
    meta <- fromMaybe def <$> AST.readMeta f
    LeftSpacedSpan (SpacedSpan _ len)
        <- view CodeSpan.realSpan <$> getLayer @CodeSpan f
    fileCode <- use Graph.code
    let apiName = Text.take (fromIntegral nameLen)
            $ Text.drop (fromIntegral nameOff) fileCode
    let code    = Code.removeMarkers $ Text.take (fromIntegral len)
            $ Text.drop (fromIntegral codeStart) fileCode
    pure $ API.ExpressionNode
            uuid
            ""
            True
            (Just apiName)
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
    inports   <- buildInPorts nid ref [] aliasPortName Nothing
    outports  <- buildOutPorts root
    code      <- Code.removeMarkers <$> getNodeCode nid
    pure $ API.ExpressionNode
        nid expr False name code inports outports meta canEnter

type TCFunResolver = Target.Target -> Maybe (IR.Term IR.Function)

buildNodeTypecheckUpdate
    :: TCFunResolver
    -> NodeId
    -> GraphOp API.NodeTypecheckerUpdate
buildNodeTypecheckUpdate funResolver nid = do
  root     <- GraphUtils.getASTPointer nid
  ref      <- GraphUtils.getASTTarget  nid
  inPorts  <- buildInPorts nid ref [] aliasPortName (Just funResolver)
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
    (_, output) <- getEdgePortMapping
    ref <- if nid == output
        then ASTRead.getCurrentASTRef
        else ASTRead.getASTTarget nid
    Code.getCodeOf ref

getDefault :: NodeRef -> GraphOp (Maybe PortDefault)
getDefault arg = match arg $ \case
        IRString s       -> Just . Constant . TextValue <$> Mutable.toList s
        IRNumber _ i f   -> do
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
        IRNumber _ i f   -> do
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
        App _ a -> do
            negLit <- isNegativeLiteral node
            if negLit then do
                posLit <- getPortState =<< source a
                let negate' (IntValue i)  = IntValue (negate i)
                    negate' (RealValue v) = RealValue (negate v)
                    negate' v             = error $
                        "getPortState: invalid negative literal: " <> show v
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
        (\(_ :: ASTRead.NoNameException) -> pure Nothing)
    pure name

resolvedDefNames :: IR.Qualified -> IR.Name -> Maybe TCFunResolver
    -> GraphOp [Maybe String]
resolvedDefNames mod function resolver = do
    let tgt = Target.Function mod function
    resolveNames tgt resolver

resolveNames :: Target.Target -> Maybe TCFunResolver -> GraphOp [Maybe String]
resolveNames tgt resolver = do
    let functionRef = ($ tgt) =<< resolver
    case functionRef of
        Just f -> extractArgNames (generalize f) resolver
        _      -> pure []

extractArgNames :: NodeRef -> Maybe TCFunResolver -> GraphOp [Maybe String]
extractArgNames node funResolve = match node $ \case
    Grouped g -> source g >>= \a -> extractArgNames a funResolve
    Lam{}     -> do
        insideLam  <- insideThisNode node
        args       <- ASTDeconstruct.extractArguments node
        vars       <- concat <$> mapM ASTRead.getVarsInside args
        let ports = if insideLam then vars else args
        mapM safeGetVarName ports
    -- App is Lam that has some args applied
    App{}     -> extractAppArgNames node funResolve
    Acc{}     -> extractAppArgNames node funResolve
    Cons{}    -> do
        vars  <- ASTRead.getVarsInside node
        names <- mapM ASTRead.getVarName vars
        pure $ map Just names
    ASGFunction _ a _ -> do
        args <- mapM source =<< ptrListToList a
        mapM safeGetVarName args
    ResolvedDef mod n -> resolvedDefNames mod n funResolve
    _ -> pure []

targetForType :: NodeRef -> IR.Name -> GraphOp Target.Target
targetForType t method = match t $ \case
    Lam _i o                   -> do
        inputType <- source o >>= getLayer @TypeLayer >>= source
        targetForType inputType method
    ResolvedCons mod klass _ _ -> pure $ Target.Method mod klass method
    _                          -> pure Target.Unknown

extractAppArgNames :: NodeRef -> Maybe TCFunResolver -> GraphOp [Maybe String]
extractAppArgNames node funResolve = go [] node where
    go :: [Maybe String] -> NodeRef -> GraphOp [Maybe String]
    go vars node' = match node' $ \case
        ResolvedDef mod n -> resolvedDefNames mod n funResolve
        App f a -> do
            varName <- safeGetVarName =<< source a
            go (varName : vars) =<< source f
        Lam{}   -> extractArgNames node' funResolve
        Cons{}  -> pure vars
        Var{}   -> pure vars
        Acc t a -> do
            argTp  <- source t >>= getLayer @TypeLayer >>= source
            method <- source a >>= ASTRead.getVarName'
            tgt    <- targetForType argTp method
            names  <- resolveNames tgt funResolve
            pure $ Safe.tailSafe names
        _       -> pure []

insideThisNode :: NodeRef -> GraphOp Bool
insideThisNode node = (== node) <$> ASTRead.getCurrentASTTarget

getPortsNames :: NodeRef -> Maybe TCFunResolver -> GraphOp [String]
getPortsNames node funResolve = do
    names <- extractArgNames node funResolve
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

extractListPorts :: NodeRef -> GraphOp [(TypeRep, PortState)]
extractListPorts n = match n $ \case
    App f a -> do
        rest <- extractListPorts =<< source f
        let addPort edge = do
                argTp <- source edge >>= getLayer @TypeLayer >>= source
                t     <- Print.getTypeRep argTp
                ps    <- getPortState =<< source edge
                pure $ (t,ps) : rest
        source a >>= flip match (\case
            Var{}      -> addPort a
            IRNumber{} -> addPort a
            IRString{} -> addPort a
            Lam{}      -> addPort a
            _ -> do
                portsInside <- extractListPorts =<< source a
                pure $ portsInside <> rest)
    Lam i o -> do
        foo <- extractListPorts =<< source i
        bar <- extractListPorts =<< source o
        pure $ foo <> bar
    ResolvedList Prepend args -> do
        args' <- ptrListToList args
        as <- mapM (source >=> extractListPorts) args'
        pure $ concat as
    _ -> do
        pure mempty

extractPortInfo :: NodeRef -> GraphOp [(TypeRep, PortState)]
extractPortInfo n = do
    tp       <- getLayer @TypeLayer n >>= source
    match tp $ \case
        ResolvedList "List" _ -> reverse <$> extractListPorts n
        _ -> do
            applied  <- reverse <$> extractAppliedPorts False False [] n
            fromType <- extractArgTypes tp
            pure $ mergePortInfo applied fromType

isNegativeLiteral :: NodeRef -> GraphOp Bool
isNegativeLiteral ref = match ref $ \case
    App f a -> do
        minus <- do
            source f >>= (flip match $ \case
                Var n -> return $ n == Parser.uminus
                _     -> return False)
        number <- do
            source a >>= (flip match $ \case
                IRNumber{} -> return True
                _        -> return False)
        return $ minus && number
    _ -> return False

pattern ResolvedList cons args <- ResolvedCons "Std.Base" "List" cons args
pattern Prepend, Empty :: (Eq a, IsString a) => a
pattern Prepend                <- "Prepend"
pattern Empty                  <- "Empty"

isPrepend :: NodeRef -> GraphOp Bool
isPrepend ref = match ref $ \case
    ResolvedList Prepend _ -> pure True
    App f _                -> source f >>= isPrepend
    Lam _ o                -> source o >>= isPrepend
    _                      -> pure False

isListLiteral :: NodeRef -> GraphOp Bool
isListLiteral ref = match ref $ \case
    ResolvedList Empty _ -> pure True
    App f a              -> do
        prepend <- source f >>= isPrepend
        literal <- source a >>= isListLiteral
        pure $ prepend && literal
    _                    -> pure False

buildArgPorts :: InPortId -> NodeRef -> Maybe TCFunResolver -> GraphOp [InPort]
buildArgPorts currentPort ref resolveFun = do
    typed     <- extractPortInfo ref
    isLiteral <- isListLiteral ref
    names     <- if isLiteral
        then pure mempty
        else getPortsNames ref resolveFun
    let portsTypes = fmap fst typed
            <> List.replicate (length names - length typed) TStar
        psCons = zipWith3 Port
            ((currentPort <>) . pure . Arg <$> [(0::Int)..])
            (map Text.pack $ names <> (("arg" <>) . show <$> [(0::Int)..]))
            portsTypes
    pure $ zipWith ($) psCons (fmap snd typed <> repeat NotConnected)

buildSelfPort ::
    NodeId -> InPortId -> NodeRef -> GraphOp (Maybe (InPortTree InPort))
buildSelfPort nid currentPort node = do
    let potentialSelf = Port currentPort selfPortName TStar NotConnected
    match node $ \case
        Acc t _ -> do
            tgt  <- source t
            tree <- buildInPorts nid tgt currentPort selfPortName Nothing
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

buildInPorts
    :: NodeId
    -> NodeRef
    -> InPortId
    -> Text
    -> Maybe TCFunResolver
    -> GraphOp (InPortTree InPort)
buildInPorts nid ref currentPort portName resolveFun = do
    negLiteral <- isNegativeLiteral ref
    if negLiteral then do
        whole    <- buildWholePort nid currentPort portName ref
        pure $ LabeledTree (InPorts def def def) whole
    else do
        selfPort <- buildSelfPort nid (currentPort <> [Self]) ref
        argPorts <- buildArgPorts currentPort ref resolveFun
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
        buildSubtrees as = zipWithM
            buildOutPortTree
            ((portId <>) . pure . Port.Projection <$> [0 ..])
            =<< mapM source as
        toSubtrees args = buildSubtrees . coerce =<< ptrListToList args
    children <- match ref $ \case
        Cons             _ args -> toSubtrees args
        Tuple              args -> toSubtrees args
        List               args -> toSubtrees args
        ResolvedCons _ _ _ args -> toSubtrees args
        _                       -> pure mempty
    pure $ LabeledTree (OutPorts children) wholePort

buildOutPorts :: NodeRef -> GraphOp (OutPortTree OutPort)
buildOutPorts ref = match ref $ \case
    Unify l _r -> buildOutPortTree [] =<< source l
    _          -> buildDummyOutPort ref


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
    API.InputSidebar nid' ps _ <- buildInputSidebar nid
    pure $ API.InputSidebarUpdate nid' ps


buildInputSidebar :: NodeId -> GraphOp API.InputSidebar
buildInputSidebar nid = do
    ref      <- ASTRead.getCurrentASTTarget
    isDef    <- ASTRead.isASGFunction ref
    args     <- ASTDeconstruct.extractFunctionPorts ref
    argTrees <- zipWithM buildOutPortTree (pure . Projection <$> [0..]) args
    pure $ API.InputSidebar nid argTrees isDef

buildOutputSidebarTypecheckUpdate :: NodeId -> GraphOp API.NodeTypecheckerUpdate
buildOutputSidebarTypecheckUpdate nid = do
    API.OutputSidebar nid' m <- buildOutputSidebar nid
    pure $ API.OutputSidebarUpdate nid' m

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
    fmap join $ forM edges $ \(_i, o) -> do
        connection <- getOutputSidebarInputs o
        pure $ (view srcNodeId . fst) <$> connection

resolveInput :: NodeRef -> GraphOp (Maybe OutPortRef)
resolveInput n = traverse fromPortMarker =<< getLayer @Marker n

deepResolveInputs ::
    NodeId -> NodeRef -> InPortRef -> GraphOp [(OutPortRef, InPortRef)]
deepResolveInputs nid ref' portRef = do
    ref                   <- ASTRead.cutThroughGroups ref'
    currentPortResolution <- toList <$> resolveInput ref
    varsInsideLambda      <- do
        refIsLam <- ASTRead.isLambda ref
        if refIsLam
        then do
            vars    <- ASTRead.getVarsInside ref
            lamArgs <- ASTDeconstruct.extractLamArguments ref
            pure $ Set.toList $
                Set.fromList vars `Set.difference` Set.fromList lamArgs
        else
            pure mempty
    inputsLam <- mapM resolveInput varsInsideLambda
    let connsInLam          = map (, portRef) $ catMaybes inputsLam
        notCurrentNode port = port ^. srcNodeId /= nid
        currentPortConn     = (, portRef)
            <$> filter notCurrentNode currentPortResolution
        unfilteredPortConn  = (, portRef) <$> currentPortResolution
    args               <- ASTDeconstruct.extractAppPorts ref
    startingPortNumber <- match ref $ \case
        LeftSection{} -> pure 1
        _             -> pure 0
    let appendPortId pid = portRef & dstPortId %~ (<> [pid])
    argsConns <- forM (zip args [startingPortNumber..]) $ \(arg, i)
        -> deepResolveInputs nid arg $ appendPortId (Arg i)
    funHead   <- ASTDeconstruct.extractFun  ref
    self      <- ASTDeconstruct.extractSelf funHead
    firstRun  <- (== ref) <$> ASTRead.getASTTarget nid
    headConns <- case (self, funHead == ref) of
        (Just s, _) -> deepResolveInputs nid s       $ appendPortId Self
        (_, False)  -> deepResolveInputs nid funHead $ appendPortId Head
        (_, True)   -> pure $ if null currentPortConn && not firstRun
            then unfilteredPortConn
            else []
    pure $ concat [currentPortConn, headConns, concat argsConns, connsInLam]

getNodeInputs :: NodeId -> GraphOp [(OutPortRef, InPortRef)]
getNodeInputs nid = do
    let loc = NodeLoc def nid
    ref      <- ASTRead.getASTTarget   nid
    deepResolveInputs nid ref (InPortRef loc [])
