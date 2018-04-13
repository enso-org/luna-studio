{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module FileModuleSpec (spec) where

import           Data.List                       (find)
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as Text
import           Empire.ASTOp                    (runASTOp)
import           Empire.ASTOps.Parse             (SomeParserException)
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph               as Graph
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (..))
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import           LunaStudio.Data.Constants       (gapBetweenNodes)
import qualified LunaStudio.Data.Graph           as APIGraph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Port            (Port (..), PortState (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (..))
import           LunaStudio.Data.PortRef         (AnyPortRef (..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.Range           (Range (..))
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))

import           Empire.Empire
import           Empire.Prelude
import           Luna.Prelude                    (forM, normalizeQQ)

import           Test.Hspec                      (Expectation, Spec, around, describe, expectationFailure, it, parallel, shouldBe,
                                                  shouldMatchList, shouldNotBe, shouldSatisfy, shouldStartWith, shouldThrow, xit)

import           EmpireUtils

import           Text.RawString.QQ               (r)


multiFunCode = [r|# Docs
def foo:
    5

# Docs
«0»def bar:
    "bar"

# Docs
def main:
    print bar
|]

multiFunCodeWithoutMarkers = [r|def foo:
    5

def bar:
    "bar"

# Docs
def main:
    print bar
|]

codeWithImport = [r|import Std

def foo:
    5

def bar:
    "bar"

def main:
    print bar
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode act env = do
    let normalize = Text.pack . normalizeQQ . Text.unpack
    actualCode <- evalEmp env $ do
        Library.createLibrary Nothing "/TestPath"
        let loc = GraphLocation "/TestPath" $ Breadcrumb []
        Graph.loadCode loc $ normalize initialCode
        main <- filter (\n -> n ^. Node.name == Just "main") <$> Graph.getNodes loc
        case main of
            [mainFun] -> do
                let loc' = GraphLocation "/TestPath" $ Breadcrumb [Definition (mainFun ^. Node.nodeId)]
                act loc'
                Graph.getCode loc'
            [] -> do
                let loc' = GraphLocation "/TestPath" $ Breadcrumb []
                act loc'
                Graph.getCode loc'
    Text.strip actualCode `shouldBe` normalize expectedCode


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "imports" $ do
        xit "adds import" $
            let initialCode = multiFunCode
                expectedCode = [r|
                    import Foo
                    # Docs
                    def foo:
                        5

                    # Docs
                    def bar:
                        "bar"

                    # Docs
                    def main:
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.addImports loc $ Set.singleton "Foo"
        xit "adds import 2" $
            let initialCode = [r|
                    import A
                    import Std
                    import Time
                    import XML

                    def foo:
                        5

                    def bar:
                        "bar"

                    def main:
                        print bar
                    |]
                expectedCode = [r|
                    import Foo
                    import A
                    import Std
                    import Time
                    import XML

                    def foo:
                        5

                    def bar:
                        "bar"

                    def main:
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.addImports loc $ Set.singleton "Foo"
        xit "adds import 3" $
            let initialCode = [r|
                    import Std.Geo
                    import Time
                    import XML

                    def foo:
                        5

                    def bar:
                        "bar"

                    def main:
                        print bar
                    |]
                expectedCode = [r|
                    import Foo
                    import Bar
                    import Baz.Quux
                    import Std.Geo
                    import Time
                    import XML

                    def foo:
                        5

                    def bar:
                        "bar"

                    def main:
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.addImports loc $ fromList ["Foo", "Bar", "Baz.Quux"]
    describe "multi-module files" $ do
        it "adds a function to empty file" $
            let initialCode = ""
                expectedCode = [r|
                    def main:
                        number1 = 4
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "def main" def
                Graph.addNode (loc |>= u1) u2 "4" def
        it "shows functions at file top-level" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                return nodes
            length nodes `shouldBe` 3
            -- nodes are layouted in a left-to-right manner
            let uniquePositions = Set.toList $ fromList $ map (view (Node.nodeMeta . NodeMeta.position . Position.x)) nodes
            length uniquePositions `shouldBe` 3
        it "adds function at top-level with arguments" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "def quux a b c" (atXPos (-1000))
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                def quux a b c:
                    None

                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "adds function at top-level as third function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "def quux" $ set NodeMeta.position (Position.fromTuple (200,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                def quux:
                    None

                # Docs
                def main:
                    print bar
                |]
        it "adds function at top-level as last function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "def quux" $ set NodeMeta.position (Position.fromTuple (500,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar

                def quux:
                    None
                |]
        it "adds function at top-level as def" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.addNode loc u1 "def quux" (atXPos (-100))
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                def quux:
                    None

                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "enters just added function" $ \env -> do
            u1 <- mkUUID
            graph <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                n <- Graph.addNode loc u1 "def quux" def
                Graph.getGraph (GraphLocation "TestPath" (Breadcrumb [Definition (n ^. Node.nodeId)]))
            length (graph ^. APIGraph.nodes) `shouldBe` 0
            let Just (Node.OutputSidebar _ ports) = graph ^. APIGraph.outputSidebar
            toListOf traverse ports `shouldBe` [Port [] "output" TStar (WithDefault (Expression "None"))]
        it "removes function at top-level" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [bar ^. Node.nodeId]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isNothing
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def main:
                    print bar
                |]
        it "removes function at top-level and undos" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [bar ^. Node.nodeId]
                Graph.addSubgraph loc [bar] []
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "renames function at top-level" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "qwerty") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isNothing
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def qwerty:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "renames function at top-level and inserts a node" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                Graph.addNode (loc |>= bar ^. Node.nodeId) u1 "1" (atXPos (-10))
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "qwerty") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isNothing
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def qwerty:
                    number1 = 1
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "renames function at top-level and inserts a node in another function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                Graph.addNode (loc |>= main ^. Node.nodeId) u1 "1" (atXPos (-10))
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def qwerty:
                    "bar"

                # Docs
                def main:
                    number1 = 1
                    print bar
                |]
        it "fails at renaming function to illegal name" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) ")" `catch` (\(_e :: SomeParserException) -> return ())
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
        it "adds and removes function" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                n <- Graph.addNode loc u1 "def quux" (atXPos (-1000))
                Graph.removeNodes loc [n ^. Node.nodeId]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "foo") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "bar") nodes `shouldSatisfy` isJust
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isNothing
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ (convert $ Code.removeMarkers multiFunCode)
        it "decodes breadcrumbs in function" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 3.14
                        «1»foo = a: b:
                            «5»lala = 17.0
                            «12»buzz = x: y:
                                «9»x * y
                            «6»pi = 3.14
                            «7»n = buzz a lala
                            «8»m = buzz b pi
                            «11»m + n
                    |]
            Breadcrumb location <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                nodes <- Graph.getNodes loc'
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                let loc'' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId)]
                Graph.decodeLocation loc''
            let names = map (view Breadcrumb.name) location
            names `shouldBe` ["main", "foo"]
        it "decodes breadcrumbs in function 2" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 3.14
                        «1»foo = a: b:
                            «5»lala = 17.0
                            «12»buzz = x: y:
                                «9»x * y
                            «6»pi = 3.14
                            «7»n = buzz a lala
                            «8»m = buzz b pi
                            «11»m + n
                    |]
            Breadcrumb location <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                nodes <- Graph.getNodes loc'
                let Just foo = find (\n -> n ^. Node.name == Just "foo") nodes
                let loc'' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId)]
                nodes <- Graph.getNodes loc''
                let Just buzz = find (\n -> n ^. Node.name == Just "buzz") nodes
                let loc''' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId), Lambda (foo ^. Node.nodeId), Lambda (buzz ^. Node.nodeId)]
                Graph.decodeLocation loc'''
            let names = map (view Breadcrumb.name) location
            names `shouldBe` ["main", "foo", "buzz"]
        it "does not crash on substitute with multiple functions" $ \env -> do
            evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.substituteCode "TestPath" [(19, 20, "10")]
        it "shows proper function offsets without imports" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                Graph.withUnit loc $ runASTOp $ forM funIds $ Code.functionBlockStart
            sort offsets `shouldBe` [0, 29, 62]
        it "shows proper function offsets with imports" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc codeWithImport
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                Graph.withUnit loc $ runASTOp $ forM funIds $ Code.functionBlockStart
            sort offsets `shouldBe` [12, 34, 60]
        it "adds node in a function with at least two nodes" $
            let initialCode = [r|
                    def foo:
                        «0»5
                    def bar:
                        «1»"bar"
                    def main:
                        «2»c = 4
                        «3»print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        number1 = 5
                        c = 4
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        it "adds node in a function with at least two nodes in a file without markers" $
            let initialCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        c = 4
                        print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        number1 = 5
                        c = 4
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        it "adds node in a function with one node" $
            let initialCode = [r|
                    def foo:
                        «0»5
                    def bar:
                        «1»"bar"
                    def main:
                        «2»print bar
                    |]
                expectedCode = [r|
                    def foo:
                        5
                    def bar:
                        "bar"
                    def main:
                        number1 = 5
                        print bar
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "5" (atXPos (-50))
        it "adds node in two functions" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= foo) u1 "5" (atXPos (-10))
                let Just bar = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "bar") nodes
                u2 <- mkUUID
                Graph.addNode (loc |>= bar) u2 "1" (atXPos (-10))
                Graph.withUnit loc $ use Graph.code
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                «1»def foo:
                    «6»number1 = 5
                    «3»5
                # Docs
                «0»def bar:
                    «7»number1 = 1
                    «4»"bar"
                # Docs
                «2»def main:
                    «5»print bar
                |]
        it "connects anonymous node" $ \env -> do
            let code = Text.pack $ normalizeQQ [r|
                    def foo:
                        n1 = _ * 5
                        5

                    def bar:
                        "bar"

                    def main:
                        print bar
                    |]
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                fooNodes <- Graph.getNodes (loc |>= foo)
                let Just n1   = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "n1") fooNodes
                    Just five = view Node.nodeId <$> find (\n -> n ^. Node.name == Nothing) fooNodes
                Graph.connect (loc |>= foo) (outPortRef five []) (InPortRef' $ inPortRef n1 [Port.Arg 0])
                Graph.withUnit loc $ use Graph.code
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                «0»def foo:
                    «4»number1 = 5
                    «3»n1 = number1 * 5
                    number1
                «1»def bar:
                    «5»"bar"
                «2»def main:
                    «6»print bar
                |]
        it "adds node after connecting to output" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "bar") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= bar) u1 "5" (atXPos 10)
                APIGraph.Graph _ _ _ (Just output) _ <- Graph.getGraph (loc |>= bar)
                Graph.connect (loc |>= bar) (outPortRef u1 []) (InPortRef' $ inPortRef (output ^. Node.nodeId) [])
                u2 <- mkUUID
                Graph.addNode (loc |>= bar) u2 "1" (atXPos (-20))
                Graph.withUnit loc $ use Graph.code
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                    # Docs
                    «1»def foo:
                        «3»5

                    # Docs
                    «0»def bar:
                        «7»number2 = 1
                        «4»text1 = "bar"
                        «6»number1 = 5
                        number1

                    # Docs
                    «2»def main:
                        «5»print bar
                |]
        it "updates block end after connecting to output" $ \env -> do
            (blockEnd, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "bar") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= bar) u1 "5" (atXPos 10)
                APIGraph.Graph _ _ _ (Just output) _ <- Graph.getGraph (loc |>= bar)
                Graph.connect (loc |>= bar) (outPortRef u1 []) (InPortRef' $ inPortRef (output ^. Node.nodeId) [])
                blockEnd <- Graph.withGraph (loc |>= bar) $ runASTOp $ Code.getCurrentBlockEnd
                code <- Graph.withUnit loc $ use Graph.code
                return (blockEnd, code)
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                    # Docs
                    «1»def foo:
                        «3»5

                    # Docs
                    «0»def bar:
                        «4»text1 = "bar"
                        «6»number1 = 5
                        number1

                    # Docs
                    «2»def main:
                        «5»print bar
                |]
            blockEnd `shouldBe` 99
        it "maintains proper block start info after adding node" $ \env -> do
            (starts, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= foo) u1 "5" (atXPos (-10))
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                starts <- Graph.withUnit loc $ runASTOp $ forM funIds $ Code.functionBlockStart
                code <- Graph.getCode loc
                return (starts, Text.unpack code)
            normalizeQQ code `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    number1 = 5
                    5

                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
            starts `shouldMatchList` [0, 48, 81]
        it "maintains proper function file offsets after adding node" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                u1 <- mkUUID
                Graph.addNode (loc |>= foo) u1 "5" (atXPos (-10))
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                offsets <- Graph.withUnit loc $ do
                    funs <- use Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo",10), ("bar",58), ("main",91)]
        it "maintains proper function file offsets after adding a function" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                u1 <- mkUUID
                Graph.addNode loc u1 "def aaa" (atXPos $ 1.5 * gapBetweenNodes)
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                offsets <- Graph.withUnit loc $ do
                    funs <- use Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo",10), ("bar",39), ("aaa",65), ("main",94)]
        it "maintains proper function file offsets after removing a function" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.removeNodes loc [bar]
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                offsets <- Graph.withUnit loc $ do
                    funs <- use Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo",10), ("main",39)]
        it "maintains proper function file offsets after renaming a function" $ \env -> do
            u1 <- mkUUID
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                funIds <- (map (view Node.nodeId)) <$> Graph.getNodes loc
                offsets <- Graph.withUnit loc $ do
                    funs <- use Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo", 10), ("qwerty", 39), ("main", 75)]
        it "adds the first function in a file" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc ""
                Graph.addNode loc u1 "def main" def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 1
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                def main:
                    None
                |]
        it "adds the first function in a file with imports" $ \env -> do
            u1 <- mkUUID
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc "import Std\nimport Foo\n"
                Graph.addNode loc u1 "def main" def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 1
            find (\n -> n ^. Node.name == Just "main") nodes `shouldSatisfy` isJust
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                import Std
                import Foo

                def main:
                    None
                |]
        it "pastes top level function with marker" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.pasteText loc [Range 23 23] ["«3»def quux: None### META {\"metas\":[{\"marker\":3,\"meta\":{\"_displayResult\":false,\"_selectedVisualizer\":null,\"_position\":{\"fromPosition\":{\"_vector2_y\":0,\"_vector2_x\":0}}}}]}"]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            normalizeQQ (Text.unpack code) `shouldBe` normalizeQQ [r|
                # Docs
                def foo:
                    5
                def quux: None
                # Docs
                def bar:
                    "bar"

                # Docs
                def main:
                    print bar
                |]
            length nodes `shouldBe` 4
        it "exports available imports" $
            let initialCode = [r|
                    import Std.Base
                    import Std.Geo

                    def main:
                        4
                    |]
            in specifyCodeChange initialCode initialCode $ \loc -> do
                imports <- Graph.getAvailableImports loc
                liftIO $ imports `shouldBe` fromList ["Std.Base", "Std.Geo", "Native"]
        it "shows implicit imports as always imported" $
            let initialCode = [r|
                    def main:
                        4
                    |]
            in specifyCodeChange initialCode initialCode $ \loc -> do
                imports <- Graph.getAvailableImports loc
                liftIO $ imports `shouldBe` fromList ["Std.Base", "Native"]
        it "changes port name on a top-level def" $
            let initialCode = [r|
                    def foo a b:
                        a + b

                    def main:
                        4
                    |]
                expectedCode = [r|
                    def foo bar baz:
                        bar + baz

                    def main:
                        4
                    |]
            in specifyCodeChange initialCode expectedCode $ \_ -> do
                let loc = GraphLocation "/TestPath" $ Breadcrumb []
                nodes <- Graph.getNodes loc
                let Just foo = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "foo") nodes
                    loc' = loc |>= foo
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.renamePort loc' (outPortRef input [Port.Projection 0]) "bar"
                Graph.renamePort loc' (outPortRef input [Port.Projection 1]) "baz"
        it "changes freshly added port name on a top-level def" $
            let initialCode = [r|
                    def foo a:
                        a

                    def main:
                        4
                    |]
                expectedCode = [r|
                    def foo a bar:
                        a

                    def main:
                        4
                    |]
            in specifyCodeChange initialCode expectedCode $ \_ -> do
                let loc = GraphLocation "/TestPath" $ Breadcrumb []
                nodes <- Graph.getNodes loc
                let Just foo = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "foo") nodes
                    loc' = loc |>= foo
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                name <- Graph.getPortName loc' (outPortRef input [Port.Projection 1])
                liftIO $ name `shouldBe` "b"
                Graph.renamePort loc' (outPortRef input [Port.Projection 1]) "bar"
        it "pastes code from text editor to node editor" $
            let initialCode = [r|
                    def main:
                        «1»number1 = 3
                        number1
                    |]
                expectedCode = [r|
                    def main:
                        number1 = 3
                        number1 = 3
                        number1
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                clipboard <- Graph.copyText loc [Range 14 25]
                Graph.paste loc (Position.fromTuple (300, 0)) $ Text.unpack clipboard
        it "pastes multiline code from text editor to node editor" $
            let initialCode = [r|
                    def main:
                        «1»number1 = 3
                        «2»node=1
                        number1
                    |]
                expectedCode = [r|
                    def main:
                        number1 = 3
                        node=1
                        number1 = 3
                        node=1
                        number1
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                clipboard <- Graph.copyText loc [Range 14 36]
                Graph.paste loc (Position.fromTuple (1000, 0)) $ Text.unpack clipboard
        it "moves ports on top-level def" $
            let initialCode = [r|
                    def main:
                        4
                    |]
                expectedCode = [r|
                    def main b foobar:
                        4
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                (input, _) <- Graph.withGraph loc $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc (outPortRef input [Port.Projection 0])
                Graph.addPort loc (outPortRef input [Port.Projection 1])
                Graph.renamePort loc (outPortRef input [Port.Projection 0]) "foobar"
                Graph.movePort loc (outPortRef input [Port.Projection 0]) 1
                Graph.movePort loc (outPortRef input [Port.Projection 1]) 0
                Graph.movePort loc (outPortRef input [Port.Projection 1]) 0
