{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module MetadataSpec (spec) where

import           Data.List                        (find)
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Empire.ASTOp                     (runASTOp)
import qualified Empire.ASTOps.Read               as ASTRead
import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Code             as Code
import qualified Empire.Commands.Graph            as Graph
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Library          as Library
import           Empire.Data.FileMetadata         (MarkerNodeMeta (MarkerNodeMeta))
import qualified Empire.Data.FileMetadata         as FileMetadata
import qualified Empire.Data.Graph                as Graph (clsClass, code, codeMarkers)
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..))
import           LunaStudio.Data.GraphLocation    (GraphLocation (..), (|>=))
import qualified LunaStudio.Data.Node             as Node
import           LunaStudio.Data.NodeMeta         (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta         as NodeMeta
import           LunaStudio.Data.Point            (Point (Point))
import qualified LunaStudio.Data.Position         as Position
import           LunaStudio.Data.TextDiff         (TextDiff (..))

import           Empire.Prelude                   hiding (pi)

import           Test.Hspec                       (Spec, around, describe, it,
                                                   parallel, shouldBe,
                                                   shouldMatchList, shouldNotBe,
                                                   shouldSatisfy, shouldStartWith)

import           EmpireUtils

import           Text.RawString.QQ                (r)



codeWithMetadata :: Text
codeWithMetadata = [r|def foo:
    «10»pi = 3.14

def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «4»lala = 17.0
        «11»buzz = x: y:
            «9»x * y
        «5»pi = 3.14
        «6»n = buzz a lala
        «7»m = buzz b pi
        «8»m + n
    «2»c = 4.0
    «3»bar = foo 8.0 c

### META {"metas":[]}
|]

withoutMetadata :: Text
withoutMetadata = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0
|]

oneNode :: Text
oneNode = [r|def main:
    «0»pi = 3.14
    None

### META {"metas":[]}
|]

simpleCodeWithMetadata :: Text
simpleCodeWithMetadata = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0

### META {"metas":[{"marker":0,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":33,"_vector2_x":66}}}},{"marker":1,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-33,"_vector2_x":-66}}}}]}
|]

testLuna :: Text
testLuna = [r|def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «5»lala = 17.0
        «12»buzz = x: y:
            «9»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «11»m + n
    «2»c = 4.0

    «14»bar = foo 8.0 c
    «17»node1 = BogusConstructorForParser
    «16»node2 = primWaitForProcess node1

### META {"metas":[]}
|]

crypto :: Text
crypto = [r|def getCurrentPrices crypto fiat:
    «0»baseUri = "https://min-api.cryptocompare.com/data/price?"
    «3»withFsym = baseUri + "fsym=" + crypto
    «4»withTsym = withFsym + "&tsyms=" + fiat
    «5»result = Http.getJSON withTsym . lookupReal fiat
    result

def main:
    «2»node1 = every 500.miliseconds (getCurrentPrices "BTC" "USD")
|]

metaWithImports :: Text
metaWithImports = [r|import Std.Base

def bar:
    «37»resp = Http.get "someurl" . perform
    «38»t1 = resp . getChunk . toText
    «39»t1 = resp . getChunk . toText

### META {"metas":[{"marker":37,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":0}}}},{"marker":38,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":176}}}},{"marker":39,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":176,"_vector2_x":176}}}}]}|]


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "handles metadata" $ do
        it "dumps metadata from a graph" $ \env -> do
            metadata <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.dumpMetadata "TestPath"
            length metadata `shouldBe` 12
        it "metadata node is deleted on file load" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isNothing
        it "shows no metadata if code doesn't have any" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc withoutMetadata
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isNothing
        it "addMetadataToCode does not overwrite current code" $ \env -> do
            (meta, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.addMetadataToCode "TestPath"
                meta <- FileMetadata.toList <$> Graph.readMetadata "TestPath"
                code <- Graph.getCode loc
                return (meta, code)
            meta `shouldMatchList` []
            code `shouldBe` Code.removeMarkers (Graph.stripMetadata codeWithMetadata)
        it "addMetadataToCode adds metadata to code without it" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc crypto
                code <- Graph.addMetadataToCode "TestPath"
                return $ Text.unpack code
            code `shouldStartWith` [r|«6»def getCurrentPrices crypto fiat:
    «0»baseUri = "https://min-api.cryptocompare.com/data/price?"
    «3»withFsym = baseUri + "fsym=" + crypto
    «4»withTsym = withFsym + "&tsyms=" + fiat
    «5»result = Http.getJSON withTsym . lookupReal fiat
    result

«7»def main:
    «2»node1 = every 500.miliseconds (getCurrentPrices "BTC" "USD")

### META|]
        it "loads metadata from a file" $ \env -> do
            (zeroMeta, oneMeta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc simpleCodeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                zeroMeta <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    ref <- preuse $ Graph.codeMarkers . ix 0
                    mapM AST.readMeta ref
                oneMeta <- Graph.withGraph (loc |>= foo ^. Node.nodeId) $ runASTOp $ do
                    ref <- preuse $ Graph.codeMarkers . ix 1
                    mapM AST.readMeta ref
                return (join zeroMeta, join oneMeta)
            zeroMeta `shouldBe` Just (NodeMeta (Position.fromTuple (66,33)) False Nothing)
            oneMeta  `shouldBe` Just (NodeMeta (Position.fromTuple (-66,-33)) False Nothing)
        it "loads crypto file" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc crypto
                nodes <- Graph.getNodes loc
                return nodes
            nodes `shouldSatisfy` (not.null)
        it "removes last node in a file with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Just pi <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    Graph.getNodeIdForMarker 0
                Graph.removeNodes (loc |>= main ^. Node.nodeId) [pi]
                Graph.getCode loc
            Text.unpack code `shouldBe` [r|def main:
    None
|]
        it "copies nodes with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                [Just c, Just bar] <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    mapM Graph.getNodeIdForMarker [2,3]
                Graph.prepareCopy (loc |>= main ^. Node.nodeId) [c, bar]
            code `shouldStartWith` [r|c = 4.0
bar = foo 8.0 c|]
        it "copies lambda with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Just foo <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    Graph.getNodeIdForMarker 1
                Graph.prepareCopy (loc |>= main ^. Node.nodeId) [foo]
            code `shouldStartWith` [r|foo = a: b:
        lala = 17.0
        buzz = x: y:
            x * y
        pi = 3.14
        n = buzz a lala
        m = buzz b pi
        m + n|]
        it "copies top level node" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.prepareCopy loc [main ^. Node.nodeId]
            code `shouldStartWith` [r|def main:
    pi = 3.14
    foo = a: b:
        lala = 17.0
        buzz = x: y:
            x * y
        pi = 3.14
        n = buzz a lala
        m = buzz b pi
        m + n
    c = 4.0
    bar = foo 8.0 c|]
        it "pastes top level node" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-10,0)) [r|def bar:
    pi = 3.14
    foo = a: b:
        lala = 17.0
        buzz = x: y:
            x * y
        pi = 3.14
        n = buzz a lala
        m = buzz b pi
        m + n
    c = 4.0
    bar = foo 8.0 c|]
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            code `shouldStartWith` [r|«2»def bar:
    «3»pi = 3.14
    «4»foo = a: b:
        «5»lala = 17.0
        «6»buzz = x: y:
            «7»x * y
        «8»pi = 3.14
        «9»n = buzz a lala
        «10»m = buzz b pi
        «11»m + n
    «12»c = 4.0
    «13»bar = foo 8.0 c
«1»def main:
    «0»pi = 3.14
    None
|]
            map (view Node.name) nodes `shouldMatchList` [Just "main", Just "bar"]
            let positions = map (view $ Node.nodeMeta . NodeMeta.position . to Position.toTuple) nodes
            length (Set.toList $ Set.fromList positions) `shouldBe` 2
        it "pastes top level node with empty line inside" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-10,0)) [r|def bar:
    pi = 3.14

    c = 4.0

    bar = foo 8.0 c|]
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            code `shouldStartWith` [r|«2»def bar:
    «3»pi = 3.14

    «4»c = 4.0

    «5»bar = foo 8.0 c
«1»def main:
    «0»pi = 3.14
    None
|]
            map (view Node.name) nodes `shouldMatchList` [Just "main", Just "bar"]
            let positions = map (view $ Node.nodeMeta . NodeMeta.position . to Position.toTuple) nodes
            length (Set.toList $ Set.fromList positions) `shouldBe` 2
        it "pastes and removes top level node" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-10,0)) [r|def foo:
    5

def bar:
    "bar"|]
                nodes <- Graph.getNodes loc
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                    Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [foo ^. Node.nodeId, bar ^. Node.nodeId]
                nodesAfter <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodesAfter, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "main"]
            code `shouldStartWith` [r|
«1»def main:
    «0»pi = 3.14
    None
|]
        it "pastes and removes and pastes top level nodes" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-500,0)) [r|def foo:
    5

def bar:
    "bar"|]
                nodes <- Graph.getNodes loc
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                    Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [foo ^. Node.nodeId, bar ^. Node.nodeId]
                Graph.paste loc (Position.fromTuple (-500,0)) [r|def foo:
    5

def bar:
    "bar"|]
                nodesAfter <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodesAfter, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "foo", Just "bar", Just "main"]
            code `shouldStartWith` [r|
«2»def foo:
    «4»5

«3»def bar:
    «5»"bar"
«1»def main:
    «0»pi = 3.14
    None
|]
        it "pastes two nodes without metadata" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.paste (loc |>= main ^. Node.nodeId) (Position.fromTuple (200,0)) [r|c = 4.0
bar = foo 8.0 c|]
                nodesAfter <- Graph.getNodes (loc |>= main ^. Node.nodeId)
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodesAfter, Text.unpack code)
            let c   = find (\n -> n ^. Node.name == Just "c") nodes
                bar = find (\n -> n ^. Node.name == Just "bar") nodes
            c `shouldSatisfy` isJust
            bar `shouldSatisfy` isJust
            code `shouldBe` [r|«1»def main:
    «0»pi = 3.14
    «2»c = 4.0
    «3»bar = foo 8.0 c
    None
|]
        it "pastes two nodes with metadata" $ \env -> do
            (code, newC, newBar) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc testLuna
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                (Just c, Just bar) <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    (,) <$> Graph.getNodeIdForMarker 2 <*> Graph.getNodeIdForMarker 14
                copy  <- Graph.prepareCopy (loc |>= main ^. Node.nodeId) [c, bar]
                Graph.paste (loc |>= main ^. Node.nodeId) (Position.fromTuple (400,0)) copy
                code  <- Graph.withUnit loc $ use Graph.code
                (newC, newBar) <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    (Just newC, Just newBar) <- (,) <$> Graph.getNodeIdForMarker 19 <*> Graph.getNodeIdForMarker 20
                    (,) <$> GraphBuilder.buildNode newC <*> GraphBuilder.buildNode newBar
                return (Text.unpack code, newC, newBar)
            newC ^. Node.nodeMeta . NodeMeta.position `shouldNotBe` newBar ^. Node.nodeMeta . NodeMeta.position
            code `shouldBe` [r|«18»def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «5»lala = 17.0
        «12»buzz = x: y:
            «9»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «11»m + n
    «2»c = 4.0
    «19»c = 4.0
    «20»bar = foo 8.0 c

    «14»bar = foo 8.0 c
    «17»node1 = BogusConstructorForParser
    «16»node2 = primWaitForProcess node1
|]
        it "moves positions to origin" $ \_ ->
            let positions = map (\pos -> MarkerNodeMeta 0 $ set NodeMeta.position (Position.fromTuple pos) def) [(-10, 30), (40, 20), (999, 222), (40, -344)]
                moved     = Graph.moveToOrigin positions
                newPositions = map (\(MarkerNodeMeta _ nm) -> nm ^. NodeMeta.position . to Position.toTuple) moved
            in  newPositions `shouldMatchList` [(0.0,374.0), (50.0,364.0), (1009.0,566.0), (50.0,0.0)]
        it "pastes lambda" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.paste (loc |>= main ^. Node.nodeId) (Position.fromTuple (200,0)) [r|«1»foo = a: b:
    «4»lala = 17.0
    «11»buzz = x: y:
        «9»x * y
    «5»pi = 3.14
    «6»n = buzz a lala
    «7»m = buzz b pi
    «8»m + n
|]
                nodesAfter <- Graph.getNodes (loc |>= main ^. Node.nodeId)
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodesAfter, Text.unpack code)
            let foo = find (\n -> n ^. Node.name == Just "foo") nodes
            foo `shouldSatisfy` isJust
            code `shouldBe` [r|«1»def main:
    «0»pi = 3.14
    «2»foo = a: b:
        «3»lala = 17.0
        «4»buzz = x: y:
            «5»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «9»m + n
    None
|]
        it "substitutes function body" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc testLuna
                --FIXME[MM]: we need this test to behave like Atom, so end column is
                --           4 characters further than it is in the file
                Graph.substituteCodeFromPoints "TestPath" $ [TextDiff (Just (Point 4 12, Point 36 14)) "5" Nothing]
                Graph.withUnit loc $ use Graph.code
            code `shouldBe` [r|«18»def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «5»lala = 17.0
        «12»buzz = x: y:
            «9»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «11»m + n
    «2»c = 4.0

    «14»5
|]
        it "removes nodes in a function in a file with imports" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc metaWithImports
                nodes <- Graph.getNodes loc
                let Just (view Node.nodeId -> bar) = find (\n -> n ^. Node.name == Just "bar") nodes
                [Just a, Just b, Just c] <- Graph.withGraph (loc |>= bar) $ runASTOp $ mapM Graph.getNodeIdForMarker [37,38,39]
                Graph.removeNodes (loc |>= bar) [b,a,c] -- this order unveiled a bug before
                Graph.withUnit loc $ use Graph.code
            code `shouldBe` [r|import Std.Base

«40»def bar:
    None
|]
