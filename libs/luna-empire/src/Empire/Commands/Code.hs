{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}

module Empire.Commands.Code where

import           Empire.Prelude
import           Control.Monad.State     (MonadState)
import           Control.Monad           (forM)
import qualified Data.Set                as Set
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Data.Text.Read          as Text
import           Data.List               (sort, sortOn)
import           Data.Maybe              (listToMaybe)
import           Empire.Data.Graph       as Graph
import           Empire.Empire           (Command, Empire)
import qualified Safe

import           Empire.Data.AST         (NodeRef, EdgeRef)
import           Empire.ASTOp            (ASTOp, ClassOp, GraphOp, runASTOp)
import           Empire.ASTOps.Read      as ASTRead

import qualified Luna.IR                 as IR
import qualified OCI.IR.Combinators      as IR (replace, substitute, replaceSource)
import           Data.Text.Position      (Delta)
import           Empire.Data.Layers      (SpanOffset, SpanLength)
import           Data.Text.Span          (LeftSpacedSpan(..), SpacedSpan(..), leftSpacedSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan, realSpan)
import qualified Luna.Syntax.Text.Parser.Marker   as Luna

import           Luna.Syntax.Text.Lexer.Grammar     (isOperator)
import qualified Luna.Syntax.Text.Lexer             as Lexer
import           Luna.Syntax.Text.Analysis.SpanTree as SpanTree

import           LunaStudio.Data.Breadcrumb         (Breadcrumb(..), BreadcrumbItem(..))
import           LunaStudio.Data.Node               (NodeId)
import qualified Empire.Data.BreadcrumbHierarchy as BH

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

extractMarkers :: Text -> Set.Set Luna.MarkerId
extractMarkers (convert -> code) = Set.fromList markers where
    markers     = mapMaybe (\(Lexer.Token _ _ symbol) -> Lexer.matchMarker symbol) lexerStream
    lexerStream = Lexer.evalDefLexer code

readMarker :: Text -> Either String Word64
readMarker text = fst <$> Text.decimal (Text.tail text)

remarkerCode :: Text -> Set.Set Luna.MarkerId -> (Text, Map.Map Luna.MarkerId Luna.MarkerId)
remarkerCode orig@(convert -> code) reservedMarkers = (remarkedCode, substitutions) where
    concatAll   subst t1 (Spanned span t2) = t1 <>
        if span ^. spanType /= MarkerSpan then t2 else (case readMarker (convert t2) of
            Right m | m `Map.member` subst ->
                let newMarker = subst Map.! m
                in convert $ makeMarker newMarker
            _ -> t2)
    lexerStream  = Lexer.evalDefLexer code
    remarkedCode = convertVia @String $ SpanTree.foldlSpans (concatAll substitutions) "" spanTree
    spanTree     = SpanTree.buildSpanTree code lexerStream
    (remarkedStream, substitutions, _) = foldl' f ([], Map.empty, reservedMarkers) lexerStream
    f :: ([Lexer.Token Lexer.Symbol], Map.Map Luna.MarkerId Luna.MarkerId, Set.Set Luna.MarkerId)
      -> Lexer.Token Lexer.Symbol
      -> ([Lexer.Token Lexer.Symbol], Map.Map Luna.MarkerId Luna.MarkerId, Set.Set Luna.MarkerId)
    f (remarkedStream, substitutions, reservedMarkers) token@(Lexer.Token s o el) =
        case el of
            Lexer.Marker m ->
                if m `Set.member` reservedMarkers then
                    let maxReservedMarker  = if Set.null reservedMarkers then 0 else Set.findMax reservedMarkers
                        newMarker          = succ maxReservedMarker
                        newSubstitutions   = Map.insert m newMarker substitutions
                        newReservedMarkers = Set.insert newMarker reservedMarkers
                        newToken           = Lexer.Token s o $ Lexer.Marker newMarker
                    in (newToken:remarkedStream, newSubstitutions, newReservedMarkers)
                else
                    (token:remarkedStream, substitutions, reservedMarkers)
            _              -> (token:remarkedStream, substitutions, reservedMarkers)

viewDeltasToReal :: Text -> (Delta, Delta) -> (Delta, Delta)
viewDeltasToReal (convert -> code) (b, e) = if b == e then (bAf, bAf) else block where
    bAf         = SpanTree.viewToRealCursorAfterMarker spantree b
    block       = SpanTree.viewToRealBlock spantree (b, e)
    spantree    = SpanTree.buildSpanTree code lexerStream
    lexerStream = Lexer.evalDefLexer code

viewToRealBlockBeforeMarker :: Spantree a -> (Delta, Delta) -> (Delta, Delta)
viewToRealBlockBeforeMarker st (left, right) = (len, r' + len - shift) where
    (len, (shift, pre, post)) = SpanTree.viewToRealCursorSplitBeforeMarker st left
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
insertAt at code = applyDiff at at code

removeAt :: (MonadState state m, Graph.HasCode state) => Delta -> Delta -> m ()
removeAt from to = void $ applyDiff from to ""

getAt :: (MonadState state m, Graph.HasCode state) => Delta -> Delta -> m Text
getAt (fromIntegral -> from) (fromIntegral -> to) = do
    code <- use Graph.code
    return $ Text.take (to - from) $ Text.drop from code

getASTTargetBeginning :: GraphOp m => NodeId -> m Delta
getASTTargetBeginning id = do
    ref      <- ASTRead.getASTRef id
    Just beg <- getOffsetRelativeToFile ref
    IR.matchExpr ref $ \case
        IR.Marked _ b' -> do
            b    <- IR.source b'
            boff <- getOffsetRelativeToTarget b'
            IR.matchExpr b $ \case
                IR.Unify l r -> do
                    roff <- getOffsetRelativeToTarget r
                    return $ boff + roff + beg
                _ -> return $ beg + boff

isOperatorVar :: GraphOp m => NodeRef -> m Bool
isOperatorVar expr = IR.matchExpr expr $ \case
    IR.Var n -> return $ isOperator n
    _        -> return False

getOffsetRelativeToTarget :: GraphOp m => EdgeRef -> m Delta
getOffsetRelativeToTarget edge = do
    ref  <- IR.readTarget edge
    let fallback = do
            inps <- IR.inputs ref
            let before = takeWhile (/= edge) inps
            lens <- forM before $ \e -> do
                off <- IR.getLayer @SpanOffset e
                len <- IR.getLayer @SpanLength =<< IR.source e
                return $ off <> len
            currentOff <- IR.getLayer @SpanOffset edge
            return $ currentOff <> foldl (<>) mempty lens
    let whenOp f a | a == edge = IR.getLayer @SpanOffset a
                   | otherwise = do
                       alen <- IR.getLayer @SpanLength =<< IR.source a
                       aoff <- IR.getLayer @SpanOffset a
                       foff <- IR.getLayer @SpanOffset f
                       return $ aoff <> alen <> foff
    IR.matchExpr ref $ \case
        IR.App f a -> do
            isOp <- isOperatorVar =<< IR.source f
            if isOp then whenOp f a else fallback
        IR.RightSection f a -> whenOp f a
        _ -> fallback


getExprMap :: GraphOp m => m (Map.Map Luna.MarkerId NodeRef)
getExprMap = use Graph.codeMarkers

setExprMap :: GraphOp m => Map.Map Luna.MarkerId NodeRef -> m ()
setExprMap exprMap = Graph.codeMarkers .= exprMap

addExprMapping :: GraphOp m => Word64 -> NodeRef -> m ()
addExprMapping index ref = do
    exprMap    <- getExprMap
    let newMap = exprMap & at index ?~ ref
    setExprMap newMap

getNextExprMarker :: GraphOp m => m Word64
getNextExprMarker = do
    globalExprMap <- use Graph.globalMarkers
    localExprMap  <- getExprMap
    let keys         = Map.keys $ Map.union globalExprMap localExprMap
        highestIndex = Safe.maximumMay keys
    return $ maybe 0 succ highestIndex

invalidateMarker :: (ASTOp g m, HasNodeCache g) => Word64 -> m ()
invalidateMarker index = do
    oldId <- use $ Graph.nodeCache . Graph.nodeIdMap . at index
    Graph.nodeCache . Graph.nodeIdMap . at index .= Nothing
    Graph.nodeCache . Graph.nodeMetaMap . at index .= Nothing
    Graph.nodeCache . Graph.portMappingMap %= Map.filterWithKey (\(nid,_) _ -> Just nid /= oldId)

addCodeMarker :: GraphOp m => Delta -> EdgeRef -> m NodeRef
addCodeMarker beg edge = do
    ref    <- IR.source edge
    index  <- getNextExprMarker
    invalidateMarker index
    marker <- IR.marker' index
    markedNode <- IR.marked' marker ref
    exprLength <- IR.getLayer @SpanLength ref
    let markerLength = convert $ Text.length $ makeMarker index
    IR.putLayer @SpanLength marker markerLength
    IR.putLayer @SpanLength markedNode (exprLength + markerLength)
    addExprMapping index markedNode
    insertAt beg (makeMarker index)
    IR.replaceSource markedNode edge
    gossipUsesChangedBy (fromIntegral $ Text.length $ makeMarker index) markedNode
    return markedNode

getOffsetRelativeToFile :: GraphOp m => NodeRef -> m (Maybe Delta)
getOffsetRelativeToFile ref = do
    begs <- getAllBeginningsOf ref
    case begs of
        [s] -> return $ Just s
        _   -> return Nothing

getAllBeginningsOf :: GraphOp m => NodeRef -> m [Delta]
getAllBeginningsOf ref = do
    succs <- toList <$> IR.getLayer @IR.Succs ref
    case succs of
        [] -> pure <$> use Graph.fileOffset
        _  -> fmap concat $ forM succs $ \s -> do
            off  <- getOffsetRelativeToTarget s
            begs <- getAllBeginningsOf =<< IR.readTarget s
            return $ (off <>) <$> begs

getAnyBeginningOf :: GraphOp m => NodeRef -> m (Maybe Delta)
getAnyBeginningOf ref = listToMaybe <$> getAllBeginningsOf ref

getCodeOf :: GraphOp m => NodeRef -> m Text
getCodeOf ref = do
    Just beg <- getAnyBeginningOf ref
    len <- IR.getLayer @SpanLength ref
    getAt beg (beg + len)

getCodeWithIndentOf :: GraphOp m => NodeRef -> m Text
getCodeWithIndentOf ref = do
    Just beg <- getAnyBeginningOf ref
    len <- IR.getLayer @SpanLength ref
    off <- getCurrentIndentationLength
    getAt (beg - off) (beg + len)

replaceAllUses :: GraphOp m => NodeRef -> Text -> m ()
replaceAllUses ref new = do
    len         <- IR.getLayer @SpanLength ref
    occurrences <- getAllBeginningsOf ref
    let fromFileEnd = reverse $ sort occurrences
    for_ fromFileEnd $ \beg -> applyDiff beg (beg + len) new
    gossipLengthsChangedBy (fromIntegral (Text.length new) - len) ref

computeLength :: GraphOp m => NodeRef -> m Delta
computeLength ref = do
    ins  <- IR.inputs ref
    case ins of
        [] -> IR.getLayer @SpanLength ref
        _  -> do
            offs <- mapM (IR.getLayer @SpanOffset) ins
            lens <- mapM (IR.getLayer @SpanLength <=< IR.source) ins
            return $ mconcat offs <> mconcat lens

functionBlockStart :: ClassOp m => NodeId -> m Delta
functionBlockStart funUUID = do
    ref  <- ASTRead.getFunByNodeId funUUID
    functionBlockStartRef ref

functionBlockStartRef :: ClassOp m => NodeRef -> m Delta
functionBlockStartRef ref = do
    LeftSpacedSpan (SpacedSpan off len) <- getOffset ref
    return $ off + len

getOffset :: ClassOp m => NodeRef -> m (LeftSpacedSpan Delta)
getOffset ref = do
    succs    <- toList <$> IR.getLayer @IR.Succs ref
    leftSpan <- case succs of
        []     -> return $ LeftSpacedSpan (SpacedSpan 0 0)
        [more] -> do
            inputs         <- IR.inputs =<< IR.readTarget more
            realInputs     <- mapM IR.readSource inputs
            let leftInputs = takeWhile (/= ref) realInputs
            moreOffset     <- getOffset =<< IR.readTarget more
            lefts          <- mconcat <$> mapM (fmap (view CodeSpan.realSpan) . IR.getLayer @CodeSpan) leftInputs
            return $ moreOffset <> lefts
    LeftSpacedSpan (SpacedSpan off _) <- view CodeSpan.realSpan <$> IR.getLayer @CodeSpan ref
    return $ leftSpan <> LeftSpacedSpan (SpacedSpan off 0)

getCurrentBlockBeginning :: GraphOp m => m Delta
getCurrentBlockBeginning = do
    tgt           <- ASTRead.getCurrentASTTarget
    Just defBegin <- getOffsetRelativeToFile tgt
    off           <- getFirstNonLambdaOffset tgt
    return $ defBegin <> off

getFirstNonLambdaOffset :: GraphOp m => NodeRef -> m Delta
getFirstNonLambdaOffset ref = IR.matchExpr ref $ \case
    IR.Lam i o -> do
        ioff  <- IR.getLayer @SpanOffset i
        ooff  <- IR.getLayer @SpanOffset o
        ilen  <- IR.getLayer @SpanLength =<< IR.source i
        recur <- getFirstNonLambdaOffset =<< IR.source o
        return $ ioff + ooff + ilen + recur
    IR.ASGFunction n as o -> getOffsetRelativeToTarget o
    _ -> return 0

getCurrentBlockEnd :: GraphOp m => m Delta
getCurrentBlockEnd = do
    body <- ASTRead.getCurrentBody
    len  <- IR.getLayer @SpanLength body
    beg  <- getCurrentBlockBeginning
    return $ len + beg

defaultIndentationLength :: Delta
defaultIndentationLength = 4

getCurrentIndentationLength :: GraphOp m => m Delta
getCurrentIndentationLength = do
      o <- getCurrentBlockBeginning
      c <- use Graph.code
      return $ fromIntegral $ Text.length $ removeMarkers $ Text.takeWhileEnd (/= '\n') $ Text.take (fromIntegral o) c

propagateLengths :: GraphOp m => NodeRef -> m ()
propagateLengths node = do
    LeftSpacedSpan (SpacedSpan off len) <- fmap (view CodeSpan.realSpan) $ IR.getLayer @CodeSpan node
    IR.putLayer @SpanLength node len
    mapM_ propagateOffsets =<< IR.inputs node

propagateOffsets :: GraphOp m => EdgeRef -> m ()
propagateOffsets edge = do
    LeftSpacedSpan (SpacedSpan off len) <- fmap (view CodeSpan.realSpan) . IR.getLayer @CodeSpan =<< IR.readSource edge
    IR.putLayer @SpanOffset edge off
    propagateLengths =<< IR.readSource edge

gossipUsesChangedBy :: GraphOp m => Delta -> NodeRef -> m ()
gossipUsesChangedBy delta ref = mapM_ (gossipLengthsChangedBy delta) =<< mapM IR.readTarget =<< (Set.toList <$> IR.getLayer @IR.Succs ref)

addToLength :: GraphOp m => NodeRef -> Delta -> m ()
addToLength ref delta = IR.modifyLayer_ @SpanLength ref (+ delta)

gossipLengthsChangedBy :: GraphOp m => Delta -> NodeRef -> m ()
gossipLengthsChangedBy delta ref = do
    addToLength ref delta
    succs     <- Set.toList <$> IR.getLayer @IR.Succs ref
    succNodes <- mapM IR.readTarget succs
    mapM_ (gossipLengthsChangedBy delta) succNodes

makeMarker :: Word64 -> Text
makeMarker s = Text.pack $ "«" <> show s <> "»"
