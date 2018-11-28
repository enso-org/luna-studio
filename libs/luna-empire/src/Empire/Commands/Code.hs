{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}

module Empire.Commands.Code where

import           Empire.Prelude          hiding (id, from, lens, span, to)
import           Control.Monad.State     (MonadState)
import           Control.Monad           (filterM, forM)
import qualified Data.Set                as Set
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Read          as Text
import           Data.List               (sort, sortOn)
import           Data.Maybe              (listToMaybe)
import qualified Empire.Data.Graph       as Graph
import qualified Safe

import           Empire.Data.AST         (NodeRef, EdgeRef)
import           Empire.ASTOp            (ASTOp, ClassOp, GraphOp)
import           Empire.ASTOps.Print     as ASTPrint
import           Empire.ASTOps.Read      as ASTRead

import qualified Luna.IR                 as IR
import           Data.Text.Position      (Delta)
import           Luna.Pass.Data.Layer.SpanLength (SpanLength)
import           Luna.Pass.Data.Layer.SpanOffset (SpanOffset)
import           Data.Text.Span          (SpacedSpan(..))
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)

import           Luna.Syntax.Text.Lexer.Grammar     (isOperator)
import qualified Luna.Syntax.Text.Lexer             as Lexer
import           Luna.Syntax.Text.Analysis.SpanTree (Spantree, Spanned(Spanned),
                                                    SpanType(MarkerSpan),
                                                    spanType)
import qualified Luna.Syntax.Text.Analysis.SpanTree as SpanTree

import           LunaStudio.Data.NodeId             (NodeId)
import qualified LunaStudio.Data.NodeCache          as NodeCache

import           LunaStudio.Data.Point (Point(Point))


pointToDelta :: Point -> Text -> Delta
pointToDelta (Point col row) code = fromIntegral $ col + row + sumOfRowsBefore where
    sumOfRowsBefore = sum $ take row rowLengths
    rowLengths = Text.length <$> Text.lines code

deltaToPoint :: Delta -> Text -> Point
deltaToPoint delta code = Point col row where
    codePrefix = Text.take (fromIntegral delta + 1) code
    row = pred $ length $ Text.lines codePrefix
    col = Text.length $ Text.takeWhileEnd (/= '\n') $ Text.init codePrefix

removeMarkers :: Text -> Text
removeMarkers (convert -> code) = convertVia @String $ SpanTree.foldlSpans concatNonMarker "" spanTree where
    spanTree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.evalDefLexer code
    concatNonMarker t (Spanned span t1) = if span ^. spanType == MarkerSpan then t else t <> t1

extractMarkers :: Text -> Set.Set Graph.MarkerId
extractMarkers (convert -> code) = Set.fromList markers where
    markers     = mapMaybe (\(Lexer.Token _ _ symbol) -> Lexer.matchMarker symbol) lexerStream
    lexerStream = Lexer.evalDefLexer code

readMarker :: Text -> Either String Word64
readMarker text = fst <$> Text.decimal (Text.tail text)

remarkerCode :: Text -> Set.Set Graph.MarkerId -> (Text, Map.Map Graph.MarkerId Graph.MarkerId)
remarkerCode (convert -> code) reservedMarkers = (remarkedCode, substitutions) where
    concatAll   subst t1 (Spanned span t2) = t1 <>
        if span ^. spanType /= MarkerSpan then t2 else (case readMarker (convert t2) of
            Right m | m `Map.member` subst ->
                let newMarker = subst Map.! m
                in convert $ makeMarker newMarker
            _ -> t2)
    lexerStream  = Lexer.evalDefLexer code
    remarkedCode = convertVia @String $ SpanTree.foldlSpans (concatAll substitutions) "" spanTree
    spanTree     = SpanTree.buildSpanTree code lexerStream
    (_remarkedStream, substitutions, _) = foldl' f ([], Map.empty, reservedMarkers) lexerStream
    f :: ([Lexer.Token Lexer.Symbol], Map.Map Graph.MarkerId Graph.MarkerId, Set.Set Graph.MarkerId)
      -> Lexer.Token Lexer.Symbol
      -> ([Lexer.Token Lexer.Symbol], Map.Map Graph.MarkerId Graph.MarkerId, Set.Set Graph.MarkerId)
    f (remarkedStream, substitutions', reservedMarkers') token@(Lexer.Token s o el) =
        case el of
            Lexer.Marker m ->
                if m `Set.member` reservedMarkers' then
                    let maxReservedMarker  = fromMaybe 0 $ Set.lookupMax reservedMarkers'
                        newMarker          = succ maxReservedMarker
                        newSubstitutions   = Map.insert m newMarker substitutions'
                        newReservedMarkers = Set.insert newMarker reservedMarkers'
                        newToken           = Lexer.Token s o $ Lexer.Marker newMarker
                    in (newToken:remarkedStream, newSubstitutions, newReservedMarkers)
                else
                    (token:remarkedStream, substitutions', reservedMarkers')
            _              -> (token:remarkedStream, substitutions', reservedMarkers')

viewDeltasToReal :: Text -> (Delta, Delta) -> (Delta, Delta)
viewDeltasToReal (convert -> code) (b, e) = if b == e then (bAf, bAf) else block where
    bAf         = SpanTree.viewToRealCursorAfterMarker spantree b
    block       = SpanTree.viewToRealBlock spantree (b, e)
    spantree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.evalDefLexer code

viewToRealBlockBeforeMarker :: Spantree a -> (Delta, Delta) -> (Delta, Delta)
viewToRealBlockBeforeMarker st (left, right) = (len, r' + len - shift) where
    (len, (shift, _, post)) = SpanTree.viewToRealCursorSplitBeforeMarker st left
    r' = SpanTree.viewToRealCursorBeforeMarker post (shift + right - left)

viewDeltasToRealBeforeMarker :: Text -> (Delta, Delta) -> (Delta, Delta)
viewDeltasToRealBeforeMarker (convert -> code) (b, e) = if b == e then (bAf, bAf) else block where
    bAf         = SpanTree.viewToRealCursorBeforeMarker spantree b
    block       = viewToRealBlockBeforeMarker spantree (b, e)
    spantree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.evalDefLexer code

-- TODO: switch to Deltas exclusively
applyDiff :: (MonadState state m, Integral a, Graph.HasCode state) => a -> a -> Text -> m Text
applyDiff (fromIntegral -> start) (fromIntegral -> end) code = do
    currentCode <- use Graph.code
    let len            = end - start
        (prefix, rest) = Text.splitAt start currentCode
        prefix'        = if Text.length prefix < start
                            then Text.concat [prefix, Text.replicate (start - Text.length prefix) " "]
                            else prefix
        suffix         = Text.drop len rest
        newCode        = Text.concat [prefix', code, suffix]
    Graph.code .= newCode
    return newCode

applyMany :: (MonadState state m, Integral a, Graph.HasCode state) => [(a, a, Text)] -> m Text
applyMany diffs = do
    mapM_ (uncurry applyDiff) $ reverse $ sortOn (view _1) diffs
    use Graph.code

insertAt :: (MonadState state m, Graph.HasCode state) => Delta -> Text -> m Text
insertAt pos code = applyDiff pos pos code

removeAt :: (MonadState state m, Graph.HasCode state) => Delta -> Delta -> m ()
removeAt from to = void $ applyDiff from to ""

getAt :: (MonadState state m, Graph.HasCode state) => Delta -> Delta -> m Text
getAt (fromIntegral -> from) (fromIntegral -> to) = do
    code <- use Graph.code
    return $ Text.take (to - from) $ Text.drop from code

getASTTargetBeginning :: NodeId -> GraphOp Delta
getASTTargetBeginning id = do
    ref      <- ASTRead.getASTRef id
    Just beg <- getOffsetRelativeToFile ref
    matchExpr ref $ \case
        Marked _ b' -> do
            b    <- source b'
            boff <- getOffsetRelativeToTarget $ generalize b'
            matchExpr b $ \case
                Unify _l r -> do
                    roff <- getOffsetRelativeToTarget $ generalize r
                    return $ boff + roff + beg
                _ -> return $ beg + boff

isOperatorVar :: NodeRef -> GraphOp Bool
isOperatorVar expr = matchExpr expr $ \case
    Var n -> return $ isOperator n
    _        -> return False

getOffsetRelativeToTarget :: EdgeRef -> GraphOp Delta
getOffsetRelativeToTarget edge = do
    ref  <- target edge
    let fallback = do
            inps <- inputs ref
            let before = takeWhile (/= edge) inps
            lens <- forM before $ \e -> do
                off <- getLayer @SpanOffset e
                len <- getLayer @SpanLength =<< source e
                return $ off <> len
            currentOff <- getLayer @SpanOffset edge
            return $ currentOff <> foldl (<>) mempty lens
    let whenOp f a | a == edge = getLayer @SpanOffset a
                   | otherwise = do
                       alen <- getLayer @SpanLength =<< source a
                       aoff <- getLayer @SpanOffset a
                       foff <- getLayer @SpanOffset f
                       return $ aoff <> alen <> foff
    matchExpr ref $ \case
        App f a -> do
            isOp <- isOperatorVar =<< source f
            if isOp then whenOp f (generalize a) else fallback
        -- RightSection f a -> whenOp f a
        _ -> fallback


getExprMap :: GraphOp (Map.Map Graph.MarkerId NodeRef)
getExprMap = use Graph.codeMarkers

setExprMap :: Map.Map Graph.MarkerId NodeRef -> GraphOp ()
setExprMap exprMap = Graph.codeMarkers .= exprMap

addExprMapping :: Word64 -> NodeRef -> GraphOp ()
addExprMapping index ref = do
    exprMap    <- getExprMap
    let newMap = exprMap & at index ?~ ref
    setExprMap newMap

getNextExprMarker :: GraphOp Word64
getNextExprMarker = do
    globalExprMap <- use Graph.globalMarkers
    localExprMap  <- getExprMap
    let keys         = Map.keys $ Map.union globalExprMap localExprMap
        highestIndex = Safe.maximumMay keys
        newMarker    = maybe 0 succ highestIndex
    invalidateMarker newMarker
    return newMarker

invalidateMarker :: Graph.HasNodeCache g => Word64 -> ASTOp g ()
invalidateMarker index = do
    oldId <- use $ Graph.nodeCache . NodeCache.nodeIdMap . at index
    Graph.nodeCache . NodeCache.nodeIdMap . at index .= Nothing
    Graph.nodeCache . NodeCache.nodeMetaMap . at index .= Nothing
    Graph.nodeCache . NodeCache.portMappingMap %= Map.filterWithKey (\(nid,_) _ -> Just nid /= oldId)

addCodeMarker :: Delta -> EdgeRef -> GraphOp NodeRef
addCodeMarker beg edge = do
    ref    <- source edge
    index  <- getNextExprMarker
    marker <- IR.marker index
    markedNode <- IR.marked' marker ref
    exprLength <- getLayer @SpanLength ref
    let markerLength = convert $ Text.length $ makeMarker index
    putLayer @SpanLength marker markerLength
    putLayer @SpanLength markedNode (exprLength + markerLength)
    addExprMapping index markedNode
    insertAt beg (makeMarker index)
    replaceSource markedNode $ generalize edge
    gossipUsesChangedBy (fromIntegral $ Text.length $ makeMarker index) markedNode
    return markedNode

getOffsetRelativeToFile :: NodeRef -> GraphOp (Maybe Delta)
getOffsetRelativeToFile ref = do
    begs <- getAllBeginningsOf ref
    case begs of
        [s] -> return $ Just s
        _   -> return Nothing

getAllBeginningsOf :: NodeRef -> GraphOp [Delta]
getAllBeginningsOf ref = do
    succs <- fmap coerce <$> ociSetToList =<< getLayer @IR.Users ref
    succsSuccs <- mapM (\a -> target a >>= \b -> getLayer @IR.Users b >>= \c -> ociSetToList c) succs
    uniSuccsSuccs <- mapM (\a -> target a >>= \b -> ASTRead.isRecord b) $ concat succsSuccs

    -- FIXME[MM]: this looks fishy, did something change regarding
    -- function successors?
    let succs' = if uniSuccsSuccs == [True] then [] else succs
    case succs' of
        [] -> do
            off <- use Graph.fileOffset
            return [off]
        _  -> fmap concat $ forM succs' $ \s -> do
            off  <- getOffsetRelativeToTarget s
            begs <- getAllBeginningsOf =<< target s
            return $ (off <>) <$> begs

getAnyBeginningOf :: NodeRef -> GraphOp (Maybe Delta)
getAnyBeginningOf ref = listToMaybe <$> getAllBeginningsOf ref

getCodeOf :: NodeRef -> GraphOp Text
getCodeOf ref = do
    Just beg <- getAnyBeginningOf ref
    len <- getLayer @SpanLength ref
    getAt beg (beg + len)

getCodeWithIndentOf :: NodeRef -> GraphOp Text
getCodeWithIndentOf ref = do
    Just beg <- getAnyBeginningOf ref
    len <- getLayer @SpanLength ref
    off <- getCurrentIndentationLength
    getAt (beg - off) (beg + len)

replaceAllUses :: NodeRef -> Delta -> Text -> GraphOp ()
replaceAllUses ref oldLen new = do
    occurrences <- getAllBeginningsOf ref
    let fromFileEnd = reverse $ sort occurrences
    for_ fromFileEnd $ \beg -> applyDiff beg (beg + oldLen) new
    gossipLengthsChangedBy (fromIntegral (Text.length new) - oldLen) ref

computeLength :: NodeRef -> GraphOp Delta
computeLength ref = do
    ins  <- inputs ref
    case ins of
        [] -> getLayer @SpanLength ref
        _  -> do
            offs <- mapM (getLayer @SpanOffset) ins
            lens <- mapM (getLayer @SpanLength <=< source) ins
            return $ mconcat offs <> mconcat lens

functionBlockStart :: NodeId -> ClassOp Delta
functionBlockStart funUUID = do
    ref  <- ASTRead.getFunByNodeId funUUID
    functionBlockStartRef ref

functionBlockStartRef :: NodeRef -> ClassOp Delta
functionBlockStartRef ref = do
    LeftSpacedSpan (SpacedSpan off len) <- getOffset ref
    return $ off + len


getOffset :: NodeRef -> ClassOp (LeftSpacedSpan Delta)
getOffset ref = do
    succs    <- ociSetToList =<< getLayer @IR.Users ref
    -- filtering ASGFunction here, because previously Var representing
    -- function name wasn't aliased with its uses inside in
    -- recursive functions
    isFun    <- mapM (target >=> ASTRead.isASGFunction) succs
    funs     <- filterM (target >=> ASTRead.isASGFunction) succs
    leftSpan <- case (if or isFun then funs else succs) of
        []     -> return $ LeftSpacedSpan (SpacedSpan 0 0)
        [more] -> do
            ins            <- inputs =<< target more
            realInputs     <- mapM source ins
            let leftInputs = takeWhile (/= ref) realInputs
            moreOffset     <- getOffset =<< target more
            lefts          <- mapM (fmap (view CodeSpan.realSpan) . getLayer @CodeSpan) leftInputs
            return $ moreOffset <> (mconcat lefts)
        _ -> ASTPrint.printFullExpression ref >>= error . ("getOffset: " <>) . convert
    LeftSpacedSpan (SpacedSpan off _) <- view CodeSpan.realSpan <$> getLayer @CodeSpan ref
    return $ leftSpan <> LeftSpacedSpan (SpacedSpan off 0)

getCurrentBlockBeginning :: GraphOp Delta
getCurrentBlockBeginning = do
    tgt           <- ASTRead.getCurrentASTTarget
    Just defBegin <- getOffsetRelativeToFile tgt
    off           <- getFirstNonLambdaOffset tgt
    return $ defBegin <> off

getFirstNonLambdaOffset :: NodeRef -> GraphOp Delta
getFirstNonLambdaOffset ref = matchExpr ref $ \case
    Lam i o -> do
        ioff  <- getLayer @SpanOffset i
        ooff  <- getLayer @SpanOffset o
        ilen  <- getLayer @SpanLength =<< source i
        recur <- getFirstNonLambdaOffset =<< source o
        return $ ioff + ooff + ilen + recur
    ASGFunction _n _as o -> getOffsetRelativeToTarget $ generalize o
    _ -> return 0

getCurrentBlockEnd :: GraphOp Delta
getCurrentBlockEnd = do
    body <- ASTRead.getCurrentBody
    len  <- getLayer @SpanLength body
    beg  <- getCurrentBlockBeginning
    return $ len + beg

defaultIndentationLength :: Delta
defaultIndentationLength = 4

getCurrentIndentationLength :: GraphOp Delta
getCurrentIndentationLength = do
      o <- getCurrentBlockBeginning
      c <- use Graph.code
      return $ fromIntegral $ Text.length $ removeMarkers $ Text.takeWhileEnd (/= '\n') $ Text.take (fromIntegral o) c

propagateLengths :: NodeRef -> GraphOp ()
propagateLengths node = do
    LeftSpacedSpan (SpacedSpan _off len) <- view CodeSpan.realSpan <$> getLayer @CodeSpan node
    putLayer @SpanLength node len
    mapM_ propagateOffsets =<< inputs node

propagateOffsets :: EdgeRef -> GraphOp ()
propagateOffsets edge = do
    LeftSpacedSpan (SpacedSpan off _len) <- fmap (view CodeSpan.realSpan) . getLayer @CodeSpan =<< source edge
    putLayer @SpanOffset edge off
    propagateLengths =<< source edge

gossipUsesChangedBy :: Delta -> NodeRef -> GraphOp ()
gossipUsesChangedBy delta ref = mapM_ (gossipLengthsChangedBy delta) =<< mapM target =<< ociSetToList =<< getLayer @IR.Users ref

addToLength :: NodeRef -> Delta -> GraphOp ()
addToLength ref delta = modifyLayer_ @SpanLength ref (+ delta)

gossipLengthsChangedBy :: Delta -> NodeRef -> GraphOp ()
gossipLengthsChangedBy delta ref = do
    addToLength ref delta
    succs     <- ociSetToList =<< getLayer @IR.Users ref
    succNodes <- mapM target succs
    mapM_ (gossipLengthsChangedBy delta) succNodes

addToLengthCls :: NodeRef -> Delta -> ClassOp ()
addToLengthCls ref delta = do
    LeftSpacedSpan (SpacedSpan off len) <- view CodeSpan.realSpan <$> getLayer @CodeSpan ref
    putLayer @CodeSpan ref $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off (len + delta)))

gossipLengthsChangedByCls :: Delta -> NodeRef -> ClassOp ()
gossipLengthsChangedByCls delta ref = do
    addToLengthCls ref delta
    succs     <- ociSetToList =<< getLayer @IR.Users ref
    succNodes <- mapM target succs
    mapM_ (gossipLengthsChangedByCls delta) succNodes

makeMarker :: Word64 -> Text
makeMarker s = Text.pack $ "«" <> show s <> "»"
