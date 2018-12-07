module Empire.Commands.Graph.Metadata where

import Empire.Prelude hiding (link)

import qualified Data.Aeson                           as Aeson
import qualified Data.Aeson.Text                      as Aeson
import qualified Data.Graph.Data.Component.Vector     as PtrList
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Map                             as Map
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified Data.Text.Lazy                       as TextLazy
import qualified Empire.ASTOps.Read                   as ASTRead
import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Code                 as Code
import qualified Empire.Data.BreadcrumbHierarchy      as BH
import qualified Empire.Data.Graph                    as Graph
import qualified Luna.IR                              as IR
import qualified Luna.Syntax.Text.Analysis.SpanTree   as SpanTree
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import qualified LunaStudio.Data.Breadcrumb           as Breadcrumb
import qualified LunaStudio.Data.GraphLocation        as GraphLocation
import qualified Safe

import Data.Text.Span                       (SpacedSpan (SpacedSpan))
import Empire.ASTOp                         (ASTOp, ClassOp, runASTOp)
import Empire.ASTOps.BreadcrumbHierarchy    (getMarker)
import Empire.Commands.Graph.Context        (withGraph, withUnit)
import Empire.Data.AST                      (EdgeRef, NodeRef)
import Empire.Data.FileMetadata             (FileMetadata (FileMetadata),
                                             MarkerNodeMeta (MarkerNodeMeta))
import Empire.Empire                        (Empire)
import Luna.Syntax.Text.Analysis.SpanTree   (Spanned (Spanned))
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb))
import LunaStudio.Data.GraphLocation        (GraphLocation (GraphLocation))
import LunaStudio.Data.NodeCache            (NodeCache (NodeCache))
import LunaStudio.Data.NodeId               (NodeId)
import LunaStudio.Data.NodeMeta             (NodeMeta)


dumpMetadata :: FilePath -> Empire [MarkerNodeMeta]
dumpMetadata file = do
    funs <- withUnit (GraphLocation.top file) $
        fmap Map.keys . use $ Graph.userState . Graph.clsFuns
    let gl fun = GraphLocation file $ Breadcrumb [Breadcrumb.Definition fun]
    metas <- forM funs $ \fun -> withGraph (gl fun) $ runASTOp $ do
        root       <- ASTRead.getCurrentBody
        oldMetas   <- extractMarkedMetasAndIds root
        pure [ MarkerNodeMeta marker meta
            | (marker, (Just meta, _)) <- oldMetas ]
    pure $ concat metas

addMetadataToCode :: FilePath -> Empire Text
addMetadataToCode file = do
    metadata <- FileMetadata <$> dumpMetadata file
    let metadataJSON
            = (TextLazy.toStrict . Aeson.encodeToLazyText . Aeson.toJSON)
                metadata
        metadataJSONWithHeader = Lexer.mkMetadata (Text.cons ' ' metadataJSON)
    withUnit (GraphLocation.top file) $ do
        code <- use $ Graph.userState . Graph.code
        let metadataJSONWithHeaderAndOffset
                = Text.cons '\n' metadataJSONWithHeader
        pure $ Text.concat [code, metadataJSONWithHeaderAndOffset]

parseMetadata :: MonadIO m => Text -> m FileMetadata
parseMetadata meta =
    let metaPrefix = "### META "
        json       = Text.drop (Text.length metaPrefix) meta
        metadata   = Aeson.eitherDecodeStrict' $ Text.encodeUtf8 json
    in case metadata of
        Right fm  -> pure fm
        Left  _   -> pure (FileMetadata mempty)

readMetadata' :: ClassOp FileMetadata
readMetadata' = do
    unit     <- use Graph.clsClass
    metaRef  <- ASTRead.getMetadataRef unit
    case metaRef of
        Just meta' -> do
            metaStart <- Code.functionBlockStartRef meta'
            LeftSpacedSpan (SpacedSpan _off len)
                <- view CodeSpan.realSpan <$> getLayer @CodeSpan meta'
            code <- Code.getAt metaStart (metaStart + len)
            parseMetadata code
        _ -> pure $ FileMetadata mempty

readMetadata :: FilePath -> Empire FileMetadata
readMetadata file = withUnit (GraphLocation.top file) $ runASTOp readMetadata'

extractMarkedMetasAndIds
    :: NodeRef -> ASTOp g [(Word64, (Maybe NodeMeta, Maybe NodeId))]
extractMarkedMetasAndIds root = matchExpr root $ \case
    Marked m e -> do
        meta    <- AST.readMeta root
        marker  <- getMarker =<< source m
        expr    <- source e
        rest    <- extractMarkedMetasAndIds expr
        var     <- ASTRead.isVar expr
        nidExpr <- ASTRead.getNodeId expr
        nidRoot <- ASTRead.getNodeId root
        let outId = if var then nidRoot else nidExpr <|> nidRoot
        pure $ (marker, (meta, outId)) : rest
    _ -> concat <$> (mapM (extractMarkedMetasAndIds <=< source) =<< inputs root)

stripMetadata :: Text -> Text
stripMetadata text = if lexerStream == code
    then text
    else flip Text.append "\n" $ Text.stripEnd $ convert withoutMeta where
        lexerStream  = Lexer.evalDefLexer (convert text)
        code = takeWhile
            (\(Lexer.Token _ _ s) -> isNothing $ Lexer.matchMetadata s)
            lexerStream
        textTree = SpanTree.buildSpanTree
            (convert text)
            $ code <> [Lexer.Token 0 0 Lexer.ETX]
        withoutMeta = SpanTree.foldlSpans
            (\t (Spanned _ t1) -> t <> t1)
            mempty
            textTree

removeMetadataNode :: ClassOp ()
removeMetadataNode = do
    unit   <- use Graph.clsClass
    klass' <- ASTRead.classFromUnit unit
    newLinks <- matchExpr klass' $ \case
        ClsASG _ _ _ _ funs'' -> do
            funs <- ptrListToList funs''
            let fromFunction f = source f >>= \f' -> matchExpr f' $ \case
                    Metadata{} -> pure Nothing
                    _          -> pure $ Just f
            catMaybes <$> mapM fromFunction funs
    Just (klass'' :: Expr (ClsASG)) <- narrowTerm klass'
    l <- PtrList.fromList (coerce newLinks)
    IR.UniTermRecord a <- Layer.read @IR.Model klass''
    let a' = a & IR.decls_Record .~ l
    void $ Layer.write @IR.Model klass'' $ IR.UniTermRecord a'

markFunctions :: NodeRef -> ClassOp ()
markFunctions unit = let
    markASGFunction f fun = do
        newMarker <- getNextTopLevelMarker
        funStart  <- Code.functionBlockStartRef f
        Code.insertAt funStart (Code.makeMarker newMarker)
        marker    <- IR.marker newMarker
        markedFun <- IR.marked' marker (generalize f)
        Graph.clsCodeMarkers . at newMarker ?= markedFun
        LeftSpacedSpan (SpacedSpan off prevLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan f
        let markerLength = convert $ Text.length $ Code.makeMarker newMarker
        putLayer @CodeSpan marker $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 markerLength))
        putLayer @CodeSpan markedFun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off prevLen))
        putLayer @CodeSpan f $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan 0 prevLen))
        asgLink <- getASGRootedFunctionLink fun
        replaceSource (coerce markedFun) (coerce asgLink)
        Code.gossipLengthsChangedByCls markerLength markedFun
    in ASTRead.classFromUnit unit >>= \klass' -> matchExpr klass' $ \case
            ClsASG _ _ _ _ funs' -> do
                funs <- map generalize <$> ptrListToList funs'
                for_ funs $ \fun -> source fun >>= \asgFun ->
                    ASTRead.cutThroughDoc asgFun >>= \f -> matchExpr f $ \case
                        ASGFunction{} -> markASGFunction f fun
                        _             -> pure ()

getASGRootedFunctionLink :: EdgeRef -> ClassOp EdgeRef
getASGRootedFunctionLink link = source link >>= \ref -> matchExpr ref $ \case
    Documented _d e -> pure $ coerce e
    Marked     _m e -> pure $ coerce e
    _               -> pure link


getNextTopLevelMarker :: ClassOp Word64
getNextTopLevelMarker = do
    globalMarkers <- use Graph.clsCodeMarkers
    let highestIndex = Safe.maximumMay $ Map.keys globalMarkers
        newMarker    = maybe 0 succ highestIndex
    Code.invalidateMarker newMarker
    pure newMarker

prepareNodeCache :: GraphLocation -> Empire NodeCache
prepareNodeCache gl = do
    let file = gl ^. GraphLocation.filePath
        toGraphLocation fun
            = GraphLocation file $ Breadcrumb [Breadcrumb.Definition fun]
    (funs, topMarkers) <- withUnit (GraphLocation.top gl) $ do
        funs       <- use $ Graph.userState . Graph.clsFuns
        topMarkers <- runASTOp $ extractMarkedMetasAndIds =<< use Graph.clsClass
        pure (Map.keys funs, Map.fromList topMarkers)
    oldMetasAndIds
        <- forM funs $ \fun -> withGraph (toGraphLocation fun) $ runASTOp $ do
            root       <- ASTRead.getCurrentBody
            oldMetas   <- extractMarkedMetasAndIds root
            pure $ Map.fromList oldMetas
    previousPortMappings
        <- forM funs $ \fun -> withGraph (toGraphLocation fun) $ runASTOp $ do
            hierarchy <- use Graph.breadcrumbHierarchy

            let lamItems = BH.getLamItems hierarchy
                elems    = lamItemToMapping ((fun, Nothing), hierarchy)
                            : map lamItemToMapping lamItems
            pure $ Map.fromList elems
    let previousNodeIds = Map.unions
            $ (Map.mapMaybe snd topMarkers)
            : (Map.mapMaybe snd <$> oldMetasAndIds)
        previousNodeMetas = Map.unions
            $ (Map.mapMaybe fst topMarkers)
            : (Map.mapMaybe fst <$> oldMetasAndIds)
    pure $ NodeCache
        previousNodeIds
        previousNodeMetas
        (Map.unions previousPortMappings)

lamItemToMapping
    :: ((NodeId, Maybe Int), BH.LamItem)
    -> ((NodeId, Maybe Int), (NodeId, NodeId))
lamItemToMapping (idArg, BH.LamItem portMapping _ _) = (idArg, portMapping)

