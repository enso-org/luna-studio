{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module MetadataSpec (spec) where

import           Control.Monad                   (forM)
import           Data.Coerce
import           Data.List                       (find)
import qualified Data.Map                        as Map
import           Data.Reflection                 (Given (..), give)
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import           Data.Text.Span                  (LeftSpacedSpan (..), SpacedSpan (..))
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Parse             as ASTParse
import qualified Empire.ASTOps.Print             as ASTPrint
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import           Empire.Data.AST                 (SomeASTException)
import qualified Empire.Data.Graph               as Graph (clsClass, clsFuns, fileOffset, code, codeMarkers, breadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Empire                   (CommunicationEnv (..), Empire)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Parser  as Parser (ReparsingChange (..), ReparsingStatus (..))
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem(Definition))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (Point))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.Vector2         (Vector2(..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))

import           Empire.Prelude
import           Luna.Prelude                    (normalizeQQ)

import           Test.Hspec                      (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                  shouldNotBe, shouldSatisfy, shouldStartWith, xit, Expectation)

import           EmpireUtils

import           Text.RawString.QQ               (r)

import qualified Luna.IR                         as IR

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

withoutMetadata = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0
|]

oneNode = [r|def main:
    «0»pi = 3.14
    None

### META {"metas":[]}
|]

simpleCodeWithMetadata = [r|def foo:
    «1»pi = 3.14

def main:
    «0»c = 4.0

### META {"metas":[{"marker":0,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":33,"_vector2_x":66}}}},{"marker":1,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-33,"_vector2_x":-66}}}}]}
|]


crypto = [r|def getCurrentPrices crypto fiat:
    «0»baseUri = "https://min-api.cryptocompare.com/data/price?"
    «3»withFsym = baseUri + "fsym=" + crypto
    «4»withTsym = withFsym + "&tsyms=" + fiat
    «5»result = Http.getJSON withTsym . lookupReal fiat
    result

def main:
    «2»node1 = every 500.miliseconds (getCurrentPrices "BTC" "USD")
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

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
        it "gets metadata expr" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isJust
        it "shows no metadata if code doesn't have any" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc withoutMetadata
                Graph.withUnit loc $ runASTOp $ do
                    cls <- use Graph.clsClass
                    ASTRead.getMetadataRef cls
            meta `shouldSatisfy` isNothing
        it "puts updated metadata in code" $ \env -> do
            (prevMeta, meta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                prevMeta <- Graph.dumpMetadata "TestPath"
                Graph.addMetadataToCode "TestPath"
                Graph.FileMetadata meta <- Graph.readMetadata "TestPath"
                return (prevMeta, meta)
            prevMeta `shouldMatchList` meta
        it "puts metadata in code without metadata" $ \env -> do
            (prevMeta, meta) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc withoutMetadata
                prevMeta <- Graph.dumpMetadata "TestPath"
                Graph.addMetadataToCode "TestPath"
                Graph.FileMetadata meta <- Graph.readMetadata "TestPath"
                return (prevMeta, meta)
            prevMeta `shouldMatchList` meta
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
        xit "removes last node in a file with metadata" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Just pi <- Graph.withGraph (loc |>= main ^. Node.nodeId) $ runASTOp $ do
                    Graph.getNodeIdForMarker 0
                Graph.removeNodes (loc |>= main ^. Node.nodeId) [pi]
                Graph.getCode loc
            code `shouldBe` normalizeQQ [r|
                def main:
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
            code `shouldStartWith` [r|«2»c = 4.0

«3»bar = foo 8.0 c

### META |]
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
            code `shouldStartWith` [r|«1»foo = a: b:
    «4»lala = 17.0
    «11»buzz = x: y:
        «9»x * y
    «5»pi = 3.14
    «6»n = buzz a lala
    «7»m = buzz b pi
    «8»m + n

### META|]
        it "copies top level node" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithMetadata
                nodes <- Graph.getNodes loc
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.prepareCopy loc [main ^. Node.nodeId]
            code `shouldStartWith` [r|def main:
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
    «3»bar = foo 8.0 c|]
        it "pastes top level node" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-10,0)) [r|def bar:
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
    «3»bar = foo 8.0 c|]
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "main", Just "bar"]
            let positions = map (view $ Node.nodeMeta . NodeMeta.position . to Position.toTuple) nodes
            length (Set.toList $ Set.fromList positions) `shouldBe` 2
            code `shouldStartWith` [r|def bar:
    «1»pi = 3.14
    «2»foo = a: b:
        «3»lala = 17.0
        «4»buzz = x: y:
            «5»x * y
        «6»pi = 3.14
        «7»n = buzz a lala
        «8»m = buzz b pi
        «9»m + n
    «10»c = 4.0
    «11»bar = foo 8.0 c

def main:
    «0»pi = 3.14
    None
|]
        it "pastes top level node with empty line inside" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc oneNode
                Graph.paste loc (Position.fromTuple (-10,0)) [r|def bar:
    «0»pi = 3.14

    «2»c = 4.0

    «3»bar = foo 8.0 c|]
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "main", Just "bar"]
            let positions = map (view $ Node.nodeMeta . NodeMeta.position . to Position.toTuple) nodes
            length (Set.toList $ Set.fromList positions) `shouldBe` 2
            code `shouldStartWith` [r|def bar:
    «1»pi = 3.14

    «2»c = 4.0

    «3»bar = foo 8.0 c

def main:
    «0»pi = 3.14
    None
|]
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
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                    Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [foo ^. Node.nodeId, bar ^. Node.nodeId]
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "main"]
            code `shouldStartWith` [r|

def main:
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
                nodes <- Graph.getNodes loc
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            map (view Node.name) nodes `shouldMatchList` [Just "foo", Just "bar", Just "main"]
            code `shouldStartWith` [r|

def foo:
    «3»5

def bar:
    «4»"bar"

def main:
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
                Graph.paste (loc |>= main ^. Node.nodeId) (Position.fromTuple (200,0)) [r|
                    «2»c = 4.0

                    «3»bar = foo 8.0 c
                    |]
                nodes <- Graph.getNodes (loc |>= main ^. Node.nodeId)
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            let c   = find (\n -> n ^. Node.name == Just "c") nodes
                bar = find (\n -> n ^. Node.name == Just "bar") nodes
            c `shouldSatisfy` isJust
            bar `shouldSatisfy` isJust
            code `shouldStartWith` [r|def main:
    «0»pi = 3.14
    «1»c = 4.0
    «2»bar = foo 8.0 c
    None
|]
        it "moves positions to origin" $ \_ ->
            let positions = map (\pos -> Graph.MarkerNodeMeta 0 $ set NodeMeta.position (Position.fromTuple pos) def) [(-10, 30), (40, 20), (999, 222), (40, -344)]
                moved     = Graph.moveToOrigin positions
                newPositions = map (\(Graph.MarkerNodeMeta _ nm) -> nm ^. NodeMeta.position . to Position.toTuple) moved
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
                nodes <- Graph.getNodes (loc |>= main ^. Node.nodeId)
                code  <- Graph.withUnit loc $ use Graph.code
                return (nodes, Text.unpack code)
            let foo = find (\n -> n ^. Node.name == Just "foo") nodes
            foo `shouldSatisfy` isJust
            code `shouldStartWith` [r|def main:
    «0»pi = 3.14
    «1»foo = a: b:
        «2»lala = 17.0
        «3»buzz = x: y:
            «4»x * y
        «5»pi = 3.14
        «6»n = buzz a lala
        «7»m = buzz b pi
        «8»m + n
    None
|]
