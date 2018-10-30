module Empire.Commands.Graph.Code where

import Empire.Prelude hiding (range, span)

import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Empire.ASTOps.Parse           as ASTParse
import qualified Empire.ASTOps.Read            as ASTRead
import qualified Empire.Commands.Code          as Code
import qualified Empire.Commands.Publisher     as Publisher
import qualified Empire.Data.Graph             as Graph
import qualified Empire.Data.Library           as Library
import qualified Empire.Data.FileMetadata      as FileMetadata
import qualified Empire.Empire                 as Empire
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified Luna.Syntax.Text.Lexer        as Lexer

import Control.Monad.Catch                  (handle)
import Data.Char                            (isDigit, isSpace)
import Data.List                            (find, findIndices)
import Data.Text.Position                   (Delta (Delta))
import Empire.ASTOp                         (runASTOp)
import Empire.ASTOps.BreadcrumbHierarchy    (getMarker)
import Empire.Commands.Graph.Autolayout     (autolayout, autolayoutTopLevel)
import Empire.Commands.Graph.Breadcrumb     (makeGraph)
import Empire.Commands.Graph.Context        (withLibrary, withUnit)
import Empire.Commands.Graph.Metadata       (markFunctions, prepareNodeCache,
                                             readMetadata', removeMetadataNode,
                                             stripMetadata)
import Empire.Empire                        (Empire)
import Luna.Syntax.Text.Parser.State.Marker (TermMap (TermMap))
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb),
                                             BreadcrumbItem (Definition))
import LunaStudio.Data.GraphLocation        (GraphLocation)
import LunaStudio.Data.NodeCache            (nodeIdMap, nodeMetaMap)
import LunaStudio.Data.Point                (Point)
import LunaStudio.Data.TextDiff             (TextDiff (TextDiff))

substituteCodeFromPoints :: FilePath -> [TextDiff] -> Empire ()
substituteCodeFromPoints path (breakDiffs -> diffs) = do
    let gl = GraphLocation.top path
    changes <- withUnit gl $ do
        oldCode <- use Graph.code
        let noMarkers    = Code.removeMarkers oldCode
            toDelta (TextDiff range code _) = case range of
                Just (start, end) ->
                    ( Code.pointToDelta start noMarkers
                    , Code.pointToDelta end noMarkers
                    , code)
                _ -> (0, fromIntegral $ Text.length noMarkers, code)
            viewToReal c = case Text.uncons c of
                Nothing        -> Code.viewDeltasToRealBeforeMarker
                Just (char, _) -> if isSpace char
                    then Code.viewDeltasToRealBeforeMarker
                    else Code.viewDeltasToReal
            toRealDelta (a,b,c) = let (a', b') = (viewToReal c) oldCode (a,b)
                in (a', b', c)
        pure $ map (toRealDelta . toDelta) diffs
    substituteCode path changes

-- | removeMarker removes marker at given position, assuming that marker
--   consists of '«', non-zero number of digits, and '»'.
removeMarker :: Text -> Int -> Text
removeMarker code markerPos =
    let (before, after) = Text.splitAt markerPos code
        dropMarker t =
            let (markerStart, rest) =
                    fromMaybe (Lexer.markerBegin, after) $ Text.uncons t
            in if markerStart == Lexer.markerBegin
                then
                    let numberDropped      = Text.dropWhile isDigit rest
                        (markerEnd, rest') = fromMaybe (Lexer.markerEnd, after)
                            $ Text.uncons numberDropped
                    in if markerEnd == Lexer.markerEnd
                        then rest'
                        else after
                else error $ "marker start is wrong: " <> [markerStart]
    in Text.concat [before, dropMarker after]

-- | This function computes offsets of each token from the beginning
--   of the file and stores them in _offset field of Token
cumulativeOffsetStream :: [Lexer.Token a] -> [Lexer.Token a]
cumulativeOffsetStream tokens = scanl1 f tokens where
    f (Lexer.Token prevSpan accOffset _) (Lexer.Token span offset lexeme)
        = Lexer.Token (span+offset) (accOffset + prevSpan) lexeme

-- | Takes a pair of consecutive tokens and determines if a marker
--   is placed in a proper position. If the second token is not a marker
--   or if it seems to be correctly placed, `Nothing` is returned.
--   If marker is determined to be incorrectly placed, `Just offset`
--   is returned, where offset is taken from the second token.
--   This function assumes that Tokens have cumulative offset inside
--   them, so offset returned is offset from the beginning of the code.
--
--   Marker is thought to be wrongly placed if it's directly preceded
--   by anything else than:
--       - beginning of code
--       - end of line
--       - block start (:)
isWrongMarker
    :: (Lexer.Token Lexer.Symbol, Lexer.Token Lexer.Symbol)
    -> Maybe Delta
isWrongMarker tokens
    | (Lexer.Token _ _ preceding,
       Lexer.Token _ offset (Lexer.Marker _)) <- tokens =
        case preceding of
            Lexer.STX        -> Nothing
            Lexer.EOL        -> Nothing
            Lexer.BlockStart -> Nothing
            _                -> Just offset
    | otherwise = Nothing

sanitizeMarkers :: Text -> Text
sanitizeMarkers text = let
    removeErroneousMarkers markers code
        = foldl' removeMarker code (reverse $ sort markers)
    lexerStream      = Lexer.evalDefLexer (convert text)
    cumulativeStream = cumulativeOffsetStream lexerStream
    markersIndices   = findIndices (== Lexer.markerBegin) $ toList text
    tokensForMarkers = fmap (\a -> (a, findToken cumulativeStream a))
        $ coerce markersIndices
    findToken stream index = find (\t -> t ^. Lexer.offset == index) stream
    erroneousMarkers =
        [ a | (a, Nothing) <- tokensForMarkers] <> wrongMarkers
    precedingTokens  = zip cumulativeStream $ tail cumulativeStream
    wrongMarkers     = catMaybes $ fmap isWrongMarker precedingTokens
    in if null erroneousMarkers
        then text
        else removeErroneousMarkers (coerce erroneousMarkers) text


substituteCode :: FilePath -> [(Delta, Delta, Text)] -> Empire ()
substituteCode path changes = do
    let gl = GraphLocation.top path
    newCode <- sanitizeMarkers <$> withUnit gl (Code.applyMany changes)
    handle
        (\(e :: SomeException)
            -> withUnit gl $ Graph.userState . Graph.clsParseError ?= e)
        $ do
            withUnit gl $ Graph.code .= newCode
            reloadCode gl newCode

loadCode :: GraphLocation -> Text -> Empire ()
loadCode gl code = do
    let topGl = GraphLocation.top gl
        file  = gl ^. GraphLocation.filePath
        libraryBody
            = Graph.userState . Empire.activeFiles . at file . traverse . Library.body
    (unit, grSt, scSt, exprMap) <- liftIO $ ASTParse.runProperParser code
    Graph.pmState . Graph.pmScheduler    .= scSt
    Graph.pmState . Graph.pmStage        .= grSt
    libraryBody   . Graph.clsClass       .= unit
    libraryBody   . Graph.clsCodeMarkers .= coerce exprMap
    libraryBody   . Graph.code           .= code
    libraryBody   . Graph.clsFuns        .= Map.empty

    (codeHadMeta, prevParseError) <- withUnit topGl $ do
        prevParseError <- use $ Graph.userState . Graph.clsParseError
        Graph.userState . Graph.clsParseError .= Nothing
        fileMetadata <- FileMetadata.toList <$> runASTOp readMetadata'
        let savedNodeMetas 
                = Map.fromList $ map FileMetadata.toTuple fileMetadata
        Graph.userState . Graph.nodeCache . nodeMetaMap
            %= (\cache -> Map.union cache savedNodeMetas)
        runASTOp $ do
            let codeWithoutMeta = stripMetadata code
            Graph.code .= codeWithoutMeta
            metaRef <- ASTRead.getMetadataRef unit
            removeMetadataNode
            for_ metaRef deepDelete
            pure (codeWithoutMeta /= code, prevParseError)
    when (codeHadMeta && isJust prevParseError) $ resendCode topGl
    functions <- withUnit topGl $ do
        klass <- use $ Graph.userState . Graph.clsClass
        runASTOp $ do
            markFunctions klass
            funs <- ASTRead.classFunctions klass
            forM funs $ \f -> ASTRead.cutThroughDoc f >>= \fun ->
                matchExpr fun $ \case
                    Marked m _e -> do
                        marker <- getMarker =<< source m
                        uuid   <- use $ Graph.nodeCache . nodeIdMap . at marker
                        pure (uuid, f)
                    _ -> do
                        pure (Nothing, f)
    for_ functions $ \(lastUUID, fun) -> do
        uuid <- withLibrary file $ fst <$> makeGraph fun lastUUID
        let glToAutolayout = gl & GraphLocation.breadcrumb
                .~ Breadcrumb [Definition uuid]
        void $ autolayout glToAutolayout
    void $ autolayoutTopLevel topGl


reloadCode :: GraphLocation -> Text -> Empire ()
reloadCode gl code = do
    nodeCache <- prepareNodeCache gl
    withUnit (GraphLocation.top gl)
        $ Graph.userState . Graph.nodeCache .= nodeCache
    loadCode gl code

breakDiffs :: [TextDiff] -> [TextDiff]
breakDiffs diffs = go [] diffs where
    go acc [] = reverse acc
    go acc (d@(TextDiff range code cursor):list) =
        case Text.span isSpace code of
            (prefix, suffix)
                | Text.null prefix -> go (d:acc) list
                | otherwise        -> let
                        rangeEnd   = fromJust (fmap snd range)
                        newRange   = Just (rangeEnd, rangeEnd)
                        whitespace = TextDiff range prefix cursor
                        onlyCode   = TextDiff newRange suffix cursor
                    in go (onlyCode:whitespace:acc) list

resendCode :: GraphLocation -> Empire ()
resendCode gl = resendCodeWithCursor gl Nothing

resendCodeWithCursor :: GraphLocation -> Maybe Point -> Empire ()
resendCodeWithCursor gl cursor = getCode gl >>= \code ->
    Publisher.notifyCodeUpdate (gl ^. GraphLocation.filePath) code cursor

getCode :: GraphLocation -> Empire Text
getCode gl
    = Code.removeMarkers <$> withUnit (GraphLocation.top gl) (use Graph.code)
