module Empire.ASTOps.Builder where

import           Control.Monad                      (foldM, replicateM, zipWithM_)
import           Data.Maybe                         (listToMaybe)
import qualified Data.Text                          as Text
import           Empire.Prelude
import qualified Safe

import           LunaStudio.Data.Node               (NodeId)
import           LunaStudio.Data.PortRef            (OutPortRef (..))
import           LunaStudio.Data.NodeLoc            (NodeLoc (..))
import qualified LunaStudio.Data.Port               as Port
import           Empire.ASTOp                       (GraphOp, match)
import           Empire.ASTOps.Deconstruct          (extractAppPorts, deconstructApp, extractFunctionPorts)
import qualified Empire.ASTOps.Read                 as ASTRead
import qualified Empire.ASTOps.Print                as ASTPrint
import qualified Empire.Commands.Code               as Code
import           Empire.Data.AST                    (EdgeRef, NodeRef, astExceptionFromException,
                                                     astExceptionToException)
import           Empire.Data.Layers                 (Marker, SpanLength, SpanOffset)
import qualified Empire.Data.Graph                  as Graph
import qualified Empire.Data.BreadcrumbHierarchy    as BH

import           Data.Text.Position      (Delta)

import qualified Luna.IR as IR
import qualified Luna.IR.Term.Ast.Invalid as IR


apps :: Expr f -> [NodeRef] -> GraphOp NodeRef
apps fun exprs = unsafeRelayout <$> foldM appAny (unsafeRelayout fun) (unsafeRelayout <$> exprs)

appAny :: NodeRef -> NodeRef -> GraphOp NodeRef
appAny = fmap generalize .: IR.app

newApplication :: NodeRef -> NodeRef -> Int -> GraphOp NodeRef
newApplication fun arg' pos = do
    blanks <- sequence $ replicate pos IR.blank
    let args = generalize blanks <> [arg']
    apps fun args

rewireApplication :: NodeRef -> NodeRef -> Int -> GraphOp NodeRef
rewireApplication fun arg' pos = do
    (tgt, oldArgs) <- deconstructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
    blanks <- replicateM (argsLength - length oldArgs) IR.blank
    let argsCmd = oldArgs <> map generalize blanks
        withNewArg = argsCmd & ix pos .~ arg'

    apps tgt withNewArg

replaceEdgeSource :: EdgeRef -> Delta -> NodeRef -> GraphOp ()
replaceEdgeSource edge beg newSrc = do
    newCode <- ASTPrint.printFullExpression newSrc
    let newCodeLen = fromIntegral $ Text.length newCode
    putLayer @SpanLength newSrc newCodeLen
    oldSrc  <- source edge
    oldLen  <- getLayer @SpanLength oldSrc
    Code.applyDiff beg (beg + oldLen) newCode
    replaceSource newSrc $ coerce edge
    deleteSubtree oldSrc
    Code.gossipLengthsChangedBy (newCodeLen - oldLen) =<< target edge

countArguments :: NodeRef -> GraphOp Int
countArguments expr = matchExpr expr $ \case
    Tuple        e   -> return . length =<< ptrListToList e
    App          f _ -> (+ 1) <$> (countArguments =<< source f)
    LeftSection  _ _ -> return 2
    RightSection _ _ -> return 1
    Grouped      g   -> countArguments =<< source g
    Acc          _ n -> countArguments =<< source n
    _                -> return 0

getArgumentOf :: NodeRef -> Delta -> GraphOp (EdgeRef, Delta)
getArgumentOf fun beg = matchExpr fun $ \case
    App _ a -> do
        off <- Code.getOffsetRelativeToTarget $ coerce a
        return (coerce a, beg + off)
    LeftSection _ a -> do
        off <- Code.getOffsetRelativeToTarget $ coerce a
        return (coerce a, beg + off)
    RightSection _ a -> do
        off <- Code.getOffsetRelativeToTarget $ coerce a
        return (coerce a, beg + off)
    Grouped g -> do
        off <- Code.getOffsetRelativeToTarget $ coerce g
        g'  <- source g
        getArgumentOf g' (beg + off)
    Tuple l' -> do
        l <- ptrListToList l'
        let t = coerce $ last l
        off <- Code.getOffsetRelativeToTarget t
        return (t, beg + off)
    f -> error $ show f

getOrCreateArgument :: EdgeRef -> Delta -> Int -> Int -> GraphOp (EdgeRef, Delta)
getOrCreateArgument currentFun codeBegin currentArgument neededArgument
    | currentArgument <= neededArgument = do
        padArgs currentFun codeBegin 1 (neededArgument - currentArgument)
        flip getArgumentOf codeBegin =<< source currentFun
    | otherwise = do
        fun <- source currentFun
        matchExpr fun $ \case
            Tuple l' -> do
                l <- ptrListToList l'
                arg  <- return (Safe.atMay l neededArgument) <?!> TupleElementOutOfBoundsException fun neededArgument
                foff <- Code.getOffsetRelativeToTarget $ coerce arg
                return (coerce arg, codeBegin + foff)
            Acc _ n -> do
                foff <- Code.getOffsetRelativeToTarget (coerce n)
                getOrCreateArgument (coerce n) (codeBegin + foff) currentArgument neededArgument
            Grouped g -> do
                foff <- Code.getOffsetRelativeToTarget (coerce g)
                getOrCreateArgument (coerce g) (codeBegin + foff) currentArgument neededArgument
            App f _ -> do
                foff <- Code.getOffsetRelativeToTarget (coerce f)
                getOrCreateArgument (coerce f) (codeBegin + foff) (pred currentArgument) neededArgument
            LeftSection f a -> do
                foff      <- Code.getOffsetRelativeToTarget (coerce f)
                argOffset <- getLayer @SpanOffset a
                padArgs (coerce f) (codeBegin + foff) argOffset 1
                newFun <- source f
                arg    <- source a
                ap     <- IR.app newFun arg
                putLayer @SpanLength ap =<< getLayer @SpanLength fun
                [f', a'] <- inputs ap
                putLayer @SpanOffset f' =<< getLayer @SpanOffset f
                putLayer @SpanOffset a' =<< getLayer @SpanOffset a
                replace ap fun
                getOrCreateArgument currentFun codeBegin currentArgument neededArgument

data TupleElementOutOfBoundsException = TupleElementOutOfBoundsException NodeRef Int
    deriving Show

instance Exception TupleElementOutOfBoundsException where
    toException = astExceptionToException
    fromException = astExceptionFromException

padArgs :: EdgeRef -> Delta -> Delta -> Int -> GraphOp ()
padArgs e beg argOffset i | i <= 0    = return ()
                          | otherwise = do
    bl     <- IR.blank
    fun    <- source e
    funIsTuple <- ASTRead.isTuple fun
    when funIsTuple $ throwM $ TupleElementOutOfBoundsException fun i
    ap     <- generalize <$> IR.app fun bl
    [f, a] <- inputs ap
    isOp   <- Code.isOperatorVar fun
    funLen <- getLayer @SpanLength fun
    let offset, blankLen :: Num a => a
        offset   = fromIntegral argOffset
        blankLen = 1
    putLayer @SpanLength ap (funLen + offset + blankLen)
    putLayer @SpanLength bl blankLen
    if isOp
        then do
            putLayer @SpanOffset f  offset
            Code.insertAt beg $ Text.cons '_' $ Text.replicate offset " "
        else do
            putLayer @SpanOffset a  offset
            Code.insertAt (beg + funLen) $ Text.snoc (Text.replicate offset " ") '_'
    replaceSource ap $ coerce e
    Code.gossipLengthsChangedBy (offset + blankLen) =<< target e
    padArgs e beg argOffset $ pred i

dropBlankArgumentsAtTailPosition :: EdgeRef -> Delta -> GraphOp ()
dropBlankArgumentsAtTailPosition e beg = do
    head' <- source e
    matchExpr head' $ \case
        Grouped g -> do
            off <- Code.getOffsetRelativeToTarget (coerce g)
            dropBlankArgumentsAtTailPosition (coerce g) (beg + off)
        App f a -> do
            arg <- source a
            isB <- ASTRead.isBlank arg
            when isB $ do
                fun  <- source f
                argOff    <- Code.getOffsetRelativeToTarget $ coerce a
                argOwnOff <- getLayer @SpanOffset a
                argLen    <- getLayer @SpanLength arg
                funOff    <- getLayer @SpanOffset f
                let toRemoveBegin  = beg + argOff - argOwnOff
                    toRemoveLength = argOwnOff + argLen + funOff
                Code.removeAt toRemoveBegin (toRemoveBegin + toRemoveLength)
                Code.gossipUsesChangedBy (-toRemoveLength) head'
                replaceSource fun $ coerce e
                deleteSubtree head'
                dropBlankArgumentsAtTailPosition e beg
        _ -> return ()

dropListElement :: NodeRef -> Int -> GraphOp NodeRef
dropListElement oldList index = match oldList $ \case
    List elts -> do
        l <- ptrListToList elts
        let (before, after) = splitAt index l
            l'              = before <> drop 1 after
        newElems <- mapM source l'
        newList <- IR.list' newElems
        putLayer @SpanLength newList =<< getLayer @SpanLength oldList
        match newList $ \case
            List elts' -> do
                elems <- ptrListToList elts'
                forM (zip l' elems) $ \(prev, curr) -> do
                    offset <- getLayer @SpanOffset prev
                    putLayer @SpanOffset curr offset
        replace newList oldList
        return newList

removeAppArgument :: EdgeRef -> Delta -> Int -> GraphOp ()
removeAppArgument funE beg pos = do
    ref <- source funE
    match ref $ \case
        List elts -> do
            l <- ptrListToList elts
            let (before, after) = splitAt pos l
                l'              = before <> drop 1 after
            listLen <- getLayer @SpanLength ref
            let removedArg = listToMaybe after
            forM removedArg $ \a -> do
                off <- Code.getOffsetRelativeToTarget $ coerce a
                spanOffset <- getLayer @SpanOffset a
                len <- getLayer @SpanLength =<< source a
                let nextArg = listToMaybe (drop 1 after)
                case nextArg of
                    Just next -> do
                        nextSpanOffset <- getLayer @SpanOffset next
                        Code.removeAt (beg+off) (beg+off+len+nextSpanOffset)
                        putLayer @SpanOffset next spanOffset
                        newList <- dropListElement ref pos
                        Code.gossipLengthsChangedBy (-(len+nextSpanOffset))
                            newList
                    _         -> do
                        Just listBeginning <- Code.getAnyBeginningOf ref
                        newList <- dropListElement ref pos
                        if null l' then do
                            Code.removeAt listBeginning (listBeginning+listLen)
                            let emptyListLit = "[]"
                                emptyLen = fromIntegral $ Text.length emptyListLit
                            Code.insertAt listBeginning emptyListLit
                            Code.gossipLengthsChangedBy
                                (-(fromIntegral listLen - emptyLen)) newList
                        else do
                            Code.removeAt (beg+off-spanOffset) (beg+off+len)
                            Code.gossipLengthsChangedBy
                                (-(len+spanOffset)) newList
            return ()
        _      -> do
            bl <- generalize <$> IR.blank
            putLayer @SpanLength bl 1
            applyFunction funE beg bl pos
            dropBlankArgumentsAtTailPosition funE beg

removeArgument :: EdgeRef -> Delta -> Port.InPortId -> GraphOp ()
removeArgument funE beg [Port.Self]  = void $ removeAccessor   funE beg
removeArgument funE beg [Port.Arg i] = removeAppArgument funE beg i
removeArgument funE beg (Port.Arg i : rest) = do
    argCount       <- countArguments =<< source funE
    (edge, argBeg) <- getOrCreateArgument funE beg (argCount - 1) i
    removeArgument edge argBeg rest
removeArgument funE beg (Port.Self : rest) = do
    (edge, tgtBeg) <- getCurrentAccTarget funE beg
    removeArgument edge tgtBeg rest
removeArgument _ _ _ = return ()

data SelfPortNotExistantException = SelfPortNotExistantException NodeRef
    deriving (Show)

instance Exception SelfPortNotExistantException where
    toException = astExceptionToException
    fromException = astExceptionFromException

unfoldM :: Monad m => (a -> m (Either b a)) -> a -> m b
unfoldM f a = do
    res <- f a
    case res of
        Left  b -> return b
        Right b -> unfoldM f b

unfoldM' :: Monad m => (a -> m (Maybe a)) -> a -> m a
unfoldM' f a = do
    res <- f a
    case res of
        Nothing -> return a
        Just a' -> unfoldM' f a'

getCurrentAccTarget :: EdgeRef -> Delta -> GraphOp (EdgeRef, Delta)
getCurrentAccTarget = curry $ unfoldM $ \(edge, codeBeg) -> do
    let passThrough e = Right . (e,) <$> ((+ codeBeg) <$> Code.getOffsetRelativeToTarget e)
    ref <- source edge
    matchExpr ref $ \case
        App f _ -> passThrough $ coerce f
        Acc t _ -> Left . (coerce t,) . (+ codeBeg) <$> Code.getOffsetRelativeToTarget (coerce t)
        _       -> return $ Left (edge, codeBeg)

ensureHasSelf :: EdgeRef -> Delta -> GraphOp ()
ensureHasSelf e beg = source e >>= flip matchExpr `id` \case
    App f _ -> do
        off <- Code.getOffsetRelativeToTarget (coerce f)
        ensureHasSelf (coerce f) $ beg + off
    Acc _ _ -> return ()
    Var n'  -> do
        let n = convertVia @String n'
        bl <- IR.blank
        putLayer @SpanLength bl 1
        v <- IR.var n'
        putLayer @SpanLength v $ fromIntegral $ Text.length n
        ac <- generalize <$> IR.acc bl v
        matchExpr ac $ \case
            Acc _ nm -> putLayer @SpanOffset nm $ fromIntegral $ Text.length " . "
        putLayer @SpanLength ac (4 + fromIntegral (Text.length n))
        oldTgt <- source e
        replaceSource ac $ coerce e
        deleteSubtree oldTgt
        Code.gossipLengthsChangedBy 4 =<< target e
        Code.insertAt beg "_ . "
        return ()
    LeftSection f a -> do
        bl    <- IR.blank
        putLayer @SpanLength bl 1
        name  <- ASTRead.getVarName' =<< source f
        v     <- IR.var name
        ac    <- IR.acc bl v
        putLayer @SpanLength ac (2 + fromIntegral (length $ nameToString name))
        a'    <- source a
        app   <- generalize <$> IR.app ac a'
        aSpan <- getLayer @SpanLength a'
        putLayer @SpanLength app (2 + fromIntegral (length $ nameToString name) + aSpan)
        oldTarget <- source e
        replaceSource app $ coerce e
        deleteSubtree oldTarget
        Code.insertAt beg "_."
        Code.gossipLengthsChangedBy 2 =<< target e
    _ -> throwM . SelfPortNotExistantException =<< source e

ensureFunctionIsValid :: Delta -> GraphOp ()
ensureFunctionIsValid outerIndentation = do
    let indentation = fromIntegral $
            outerIndentation + Code.defaultIndentationLength
    self        <- ASTRead.cutThroughDocAndMarked
        =<< use (Graph.breadcrumbHierarchy . BH.self)
    let whitespace = Text.replicate indentation " "
    match self $ \case
        ASGFunction n _as b -> do
            name' <- source n >>= \a -> matchExpr a $ \case
                Invalid IR.MissingFunctionName -> pure $ Just " func"
                Invalid IR.InvalidFunctionName -> pure Nothing
                Invalid inv                    ->
                    error ("ensureFunctionIsValid: " <> show inv)
                _                              -> pure Nothing
            for_ name' $ \(name :: String) -> do
                var <- IR.var $ convert name
                let nameLength = fromIntegral $ length name
                putLayer @SpanLength var nameLength
                Just namePos <- Code.getOffsetRelativeToFile =<< source n
                Code.insertAt namePos $ convert name
                IR.replace var =<< source n
                Code.gossipLengthsChangedBy nameLength =<< target n
            let none :: IsString a => a
                none     = "None"
                noneLine = whitespace <> none
            section' <- source b >>= \a -> matchExpr a $ \case
                Invalid IR.MissingSection  -> pure $ Just $ ":\n" <> noneLine
                Invalid IR.EmptyExpression -> pure $ Just $ "\n"  <> noneLine
                Invalid inv                -> error ("ensureFunctionIsValid: " 
                                                    <> show inv)
                _                          -> pure Nothing
            for_ section' $ \section -> do
                ir <- IR.cons (stringToName none) []
                putLayer @SpanLength ir $
                    fromIntegral $ length none
                Just funBeg <- Code.getOffsetRelativeToFile self
                len         <- getLayer @SpanLength self
                Code.applyDiff (funBeg+len) (funBeg+len) section
                IR.replace ir =<< source b
                matchExpr self $ \case
                    ASGFunction _ _ body -> putLayer @SpanOffset body $
                        fromIntegral $ length (":\n" :: String) + indentation
                Code.gossipLengthsChangedBy (fromIntegral $ Text.length section)
                    =<< target b
        _ -> return ()


makeAccessor :: NodeRef -> EdgeRef -> Delta -> GraphOp ()
makeAccessor tgt naming exprBegin = do
    ensureHasSelf naming exprBegin
    (edge, tgtBegin) <- getCurrentAccTarget naming exprBegin
    replaceEdgeSource edge tgtBegin tgt

applyFunction :: EdgeRef -> Delta -> NodeRef -> Int -> GraphOp ()
applyFunction funE beg arg pos = do
    argCount       <- countArguments =<< source funE
    (edge, argBeg) <- getOrCreateArgument funE beg (argCount - 1) pos
    replaceEdgeSource edge argBeg arg

data ListElementOutOfBoundsException = ListElementOutOfBoundsException NodeRef Int
    deriving Show

instance Exception ListElementOutOfBoundsException where
    toException = astExceptionToException
    fromException = astExceptionFromException

type ReplacedArgInfo = (Delta, Delta, Delta)

addOrReplaceListArg :: NodeRef -> Int -> NodeRef -> GraphOp (NodeRef, Maybe ReplacedArgInfo)
addOrReplaceListArg oldList pos newArg = match oldList $ \case
    List elts -> do
        l <- ptrListToList elts
        let (before, after) = splitAt pos l
        replacedSpan <- case Safe.headMay after of
            Just replacedEdge -> do
                argBeg <- Code.getOffsetRelativeToTarget $ unsafeRelayout replacedEdge
                argLen <- getLayer @SpanLength =<< source replacedEdge
                argOff <- getLayer @SpanOffset replacedEdge
                return $ Just (argBeg, argLen, argOff)
            _ -> return Nothing
        beforeArgs <- mapM source before
        afterArgs  <- mapM source $ drop 1 after
        let newArgs = beforeArgs <> [newArg] <> afterArgs
        newList <- IR.list' newArgs
        putLayer @SpanLength newList =<< getLayer @SpanLength oldList
        match newList $ \case
            List elts' -> do
                elems <- ptrListToList elts'
                forM (zip l elems) $ \(prev, curr) -> do
                    offset <- getLayer @SpanOffset prev
                    putLayer @SpanOffset curr offset
        replace newList oldList
        return (newList, replacedSpan)

makeConnection :: EdgeRef -> Delta -> Port.InPortId -> NodeRef -> GraphOp ()
makeConnection funE beg [] arg = do
    replaceEdgeSource funE beg arg
makeConnection funE beg (Port.Self : rest) arg = do
    ensureHasSelf funE beg
    (edge, tgtBeg) <- getCurrentAccTarget funE beg
    makeConnection edge tgtBeg rest arg
makeConnection funE beg (Port.Arg i : rest) arg = source funE >>= flip match (\case
    List elts -> do
        l <- ptrListToList elts
        ref <- source funE
        when_ (i < 0 || i > length l) $
            throwM $ ListElementOutOfBoundsException ref i
        newArgCode <- ASTPrint.printFullExpression arg
        (newList, replacedSpan) <- addOrReplaceListArg ref i arg
        newArgs <- match newList $ \case
            List elts' -> ptrListToList elts'
        ithArg <- return (Safe.atMay newArgs i) <?!>
            ListElementOutOfBoundsException newList i
        case replacedSpan of
            Just (argBeg, argLen, argOff) -> do
                putLayer @SpanOffset ithArg argOff
                Code.applyDiff (beg+argBeg) (beg+argBeg+argLen) newArgCode
                Code.gossipLengthsChangedBy
                    (fromIntegral (Text.length newArgCode) - argLen) newList
            _ -> do
                -- new arg at the end, nothing to replace
                let prevArg = Safe.atMay newArgs (i - 1)
                case prevArg of
                    Just prev -> do
                        let newCode = ", " <> newArgCode
                            newCodeLength = fromIntegral $ Text.length newCode
                            commaSpaceLen = fromIntegral $
                                length (", " :: String)
                        putLayer @SpanOffset ithArg commaSpaceLen
                        lastArgBeg <- Code.getOffsetRelativeToTarget $
                            unsafeRelayout prev
                        lastArgLen <- getLayer @SpanLength =<< source prev
                        Code.insertAt (beg+lastArgBeg+lastArgLen) newCode
                        Code.gossipLengthsChangedBy newCodeLength newList
                    _ -> do
                        -- no previous arg, adding expr + ", " at the beginning
                        -- and changing spanoffset of next arg
                        let newCode = newArgCode <>
                                if length newArgs == 1 then "" else ", "
                            newCodeLength = fromIntegral $ Text.length newCode
                            squareBracket =
                                fromIntegral $ length ("[" :: String)
                        putLayer @SpanOffset ithArg squareBracket
                        Code.insertAt (beg+squareBracket) newCode
                        Code.gossipLengthsChangedBy newCodeLength newList
        return ()
    _ -> do
        argCount <- countArguments =<< source funE
        (edge, argBeg) <- getOrCreateArgument funE beg (argCount - 1) i
        makeConnection edge argBeg rest arg)
makeConnection _ _ _ _ = return ()


data SelfPortNotConnectedException = SelfPortNotConnectedException NodeRef
    deriving (Show)

instance Exception SelfPortNotConnectedException where
    toException = astExceptionToException
    fromException = astExceptionFromException

removeAccessor :: EdgeRef -> Delta -> GraphOp ()
removeAccessor ed beg = do
    expr <- source ed
    matchExpr expr $ \case
        App f _ -> do
            off <- Code.getOffsetRelativeToTarget $ coerce f
            removeAccessor (coerce f) $ beg + off
        Acc _ n -> do
            len <- getLayer @SpanLength expr
            acc <- source n
            accCode <- Code.getCodeOf =<< source n
            Code.applyDiff beg (beg + len) accCode
            replaceSource acc $ coerce ed
            deleteSubtree expr
            Code.gossipLengthsChangedBy
                (fromIntegral (Text.length accCode) - len) =<< target ed
        Grouped g -> do
            off <- Code.getOffsetRelativeToTarget (coerce g)
            removeAccessor (coerce g) (beg + off)
        _ -> return ()

detachNodeMarkers :: NodeRef -> GraphOp ()
detachNodeMarkers ref' = do
    ref <- ASTRead.cutThroughGroups ref'
    putLayer @Marker ref Nothing
    inps <- inputs ref
    mapM_ (source >=> detachNodeMarkers) inps

attachNodeMarkers :: NodeId -> Port.OutPortId -> NodeRef -> GraphOp ()
attachNodeMarkers marker port ref' = go port ref' where
    goOn :: Port.OutPortId -> [NodeRef] -> GraphOp ()
    goOn p args = zipWithM_ go ((p <>) . pure . Port.Projection <$> [0..]) args
    go :: Port.OutPortId -> NodeRef -> GraphOp ()
    go p r = do
        ref <- ASTRead.cutThroughGroups r
        match ref $ \case
            Cons _ as -> goOn p =<< mapM source =<< ptrListToList as
            App{}     -> goOn p =<< extractAppPorts ref
            Tuple as  -> goOn p =<< mapM source =<< ptrListToList as
            List  as  -> goOn p =<< mapM source =<< ptrListToList as
            _         -> putLayer @Marker ref . Just =<< toPortMarker (OutPortRef (NodeLoc def marker) p)

detachNodeMarkersForArgs :: NodeRef -> GraphOp ()
detachNodeMarkersForArgs lam = do
    args <- extractFunctionPorts lam
    mapM_ detachNodeMarkers args

attachNodeMarkersForArgs :: NodeId -> Port.OutPortId -> NodeRef -> GraphOp ()
attachNodeMarkersForArgs nid _ lam = do
    args <- extractFunctionPorts lam
    zipWithM_ (attachNodeMarkers nid) (pure . Port.Projection <$> [0..]) args

data CannotFlipNodeException = CannotFlipNodeException deriving (Show)
instance Exception CannotFlipNodeException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

patternify :: NodeRef -> GraphOp NodeRef
patternify ref = match ref $ \case
    App _ _ -> do
        (fun, args) <- deconstructApp ref
        match fun $ \case
            Cons n _ -> do
                as <- mapM patternify args
                generalize <$> IR.cons n as
            _ -> throwM CannotFlipNodeException
    Var n -> generalize <$> IR.var n -- Copy the Var to forget all metadata like node markers, potentially belonging to other nodes and unified by Alias Analysis
    Grouped g -> patternify =<< source g
    IRNumber{}  -> return ref
    IRString _  -> return ref
    Cons _ _  -> return ref
    _         -> throwM CannotFlipNodeException

appify :: NodeRef -> GraphOp NodeRef
appify ref = match ref $ \case
    Cons n as -> do
        args <- mapM (source >=> appify) =<< ptrListToList as
        fun  <- IR.cons n []
        apps fun args
    Var n     -> generalize <$> IR.var n -- Copy the Var to forget all metadata like node markers, potentially belonging to other nodes and unified by Alias Analysis
    IRNumber{}  -> return ref
    IRString _  -> return ref
    Grouped g -> appify =<< source g
    _         -> throwM CannotFlipNodeException

flipNodeCode :: NodeId -> GraphOp ()
flipNodeCode nid = do
    lhs          <- ASTRead.getASTVar    nid
    rhs          <- ASTRead.getASTTarget nid
    lhsCode      <- Code.getCodeOf lhs
    rhsCode      <- Code.getCodeOf rhs
    targetOffset <- getLayer @SpanOffset =<< ASTRead.getTargetEdge nid
    Just lhsBeg  <- Code.getAnyBeginningOf lhs
    Just rhsBeg  <- Code.getAnyBeginningOf rhs
    lhsLen       <- getLayer @SpanLength lhs
    rhsLen       <- getLayer @SpanLength rhs
    Code.removeAt rhsBeg (rhsBeg + rhsLen)
    Code.removeAt lhsBeg (lhsBeg + lhsLen)
    Code.insertAt (lhsBeg + targetOffset) lhsCode
    void $ Code.insertAt lhsBeg rhsCode

flipNode :: NodeId -> GraphOp ()
flipNode nid = do
    ref    <- ASTRead.getASTPointer nid
    lhs    <- ASTRead.getASTVar    nid
    rhs    <- ASTRead.getASTTarget nid
    targetOffset <- getLayer @SpanOffset =<< ASTRead.getTargetEdge nid
    flipNodeCode nid
    let copySpanLength recipient donor =
            putLayer @SpanLength recipient =<< getLayer @SpanLength donor
    newlhs <- patternify rhs
    newrhs <- appify     lhs
    uni    <- IR.unify newlhs newrhs
    copySpanLength newlhs rhs
    copySpanLength newrhs lhs
    copySpanLength uni ref
    attachNodeMarkers nid [] newlhs
    replace uni ref
    newEdge <- ASTRead.getTargetEdge nid
    putLayer @SpanOffset newEdge targetOffset

attachName :: NodeRef -> Text -> GraphOp (NodeRef, NodeRef)
attachName node n = do
    var <- IR.var' $ convertVia @String n
    putLayer @SpanLength var (convert $ Text.length n)
    uni    <- IR.unify' var node
    [l, r] <- inputs uni
    putLayer @SpanOffset l 0
    putLayer @SpanOffset r 3
    putLayer @SpanLength uni =<< Code.computeLength uni
    return (var, uni)


ensureNodeHasName :: (NodeRef -> GraphOp Text) -> NodeId -> GraphOp ()
ensureNodeHasName generateNodeName nid = do
    ref <- ASTRead.getASTRef nid
    matchExpr ref $ \case
        Marked _ e -> do
            expr  <- source e
            match expr $ \case
                Unify{} -> return ()
                ASGFunction{} -> return ()
                _ -> do
                    name     <- generateNodeName expr
                    (var, uni) <- attachName expr name
                    replaceSource uni $ coerce e
                    Just codeBeg <- Code.getOffsetRelativeToFile ref
                    off          <- Code.getOffsetRelativeToTarget $ coerce e
                    Code.insertAt (codeBeg + off) (name <> " = ")
                    Code.gossipUsesChangedBy (fromIntegral $ Text.length name + 3) uni
                    attachNodeMarkers nid [] var
        _ -> throwM $ ASTRead.MalformedASTRef ref

makeNodeRep :: NodeId -> Maybe Text -> GraphOp Text -> NodeRef -> GraphOp (NodeRef, Maybe Text)
makeNodeRep marker name generateNodeName node = do
    (pat, uni, newName) <- match node $ \case
        Unify l _r        -> (, node, Nothing) <$> source l
        ASGFunction n _ _ -> (, node, Nothing) <$> source n
        _                 -> do
            n   <- maybe generateNodeName pure name
            (var, uni) <- attachName node n
            return (var, uni, Just n)
    attachNodeMarkers marker [] pat
    return (uni, newName)
