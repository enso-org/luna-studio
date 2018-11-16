{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module FileModuleSpec (spec) where

import           Control.Lens                    (toListOf)
import           Data.List                       (find)
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import           Empire.ASTOp                    (runASTOp)
import           Empire.ASTOps.Parse             (SomeParserException)
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import qualified Empire.Data.Graph               as Graph
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (..))
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import           LunaStudio.Data.Constants       (gapBetweenNodes)
import qualified LunaStudio.Data.Graph           as APIGraph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..), (|>=))
import qualified LunaStudio.Data.Node            as Node
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (..))
import           LunaStudio.Data.Port            (Port (..), PortState (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (..))
import           LunaStudio.Data.PortRef         (AnyPortRef (..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.Range           (Range (..))
import           LunaStudio.Data.TextDiff        (TextDiff (..))
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))

import           Empire.Prelude                  as P

import           Test.Hspec                      (Spec, around, describe, it,
                                                  parallel, shouldBe,
                                                  shouldMatchList, shouldSatisfy)

import           EmpireUtils

import           Text.RawString.QQ               (r)


multiFunCode :: Text
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

codeWithImport :: Text
codeWithImport = [r|import Std

def foo:
    5

def bar:
    "bar"

def main:
    print bar
|]

atXPos :: Double -> NodeMeta.NodeMeta
atXPos = ($ def) . (NodeMeta.position . Position.x .~)


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "imports" $ do
        it "adds import" $
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
        it "adds import 2" $
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
        it "adds import 3" $
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
                    import Bar
                    import Baz.Quux
                    import Foo
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
        it "adds a function to empty file 2" $
            let initialCode = ""
                expectedCode = [r|
                    def main:
                        number1 = 4
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "def main:" def
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                Graph.addNode loc u1 "def quux" $ set NodeMeta.position (Position.fromTuple (400,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                Graph.addNode loc u1 "def quux" $ set NodeMeta.position (Position.fromTuple (800,0)) def
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            length nodes `shouldBe` 4
            find (\n -> n ^. Node.name == Just "quux") nodes `shouldSatisfy` isJust
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                let Just main = find (\n -> n ^. Node.name == Just "main") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                Graph.addNode (loc |>= main ^. Node.nodeId) u1 "1" (atXPos (-10))
                Graph.getCode loc
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode (Code.removeMarkers multiFunCode)
        it "decodes breadcrumbs in function" $ \env -> do
            let code = normalizeLunaCode $ [r|
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
            let code = normalizeLunaCode $ [r|
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
                nodesFoo <- Graph.getNodes loc''
                let Just buzz = find (\n -> n ^. Node.name == Just "buzz") nodesFoo
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            let initialCode = normalizeLunaCode [r|
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
                Graph.loadCode loc initialCode
                nodes <- Graph.getNodes loc
                let Just foo = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "foo") nodes
                fooNodes <- Graph.getNodes (loc |>= foo)
                let Just n1   = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "n1") fooNodes
                    Just five = view Node.nodeId <$> find (\n -> n ^. Node.name == Nothing) fooNodes
                Graph.connect (loc |>= foo) (outPortRef five []) (InPortRef' $ inPortRef n1 [Port.Arg 0])
                Graph.withUnit loc $ use Graph.code
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                APIGraph.Graph _ _ _ (Just output) _ _ <- Graph.getGraph (loc |>= bar)
                Graph.connect (loc |>= bar) (outPortRef u1 []) (InPortRef' $ inPortRef (output ^. Node.nodeId) [])
                u2 <- mkUUID
                Graph.addNode (loc |>= bar) u2 "1" (atXPos (-20))
                Graph.withUnit loc $ use Graph.code
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                APIGraph.Graph _ _ _ (Just output) _ _ <- Graph.getGraph (loc |>= bar)
                Graph.connect (loc |>= bar) (outPortRef u1 []) (InPortRef' $ inPortRef (output ^. Node.nodeId) [])
                blockEnd <- Graph.withGraph (loc |>= bar) $ runASTOp $ Code.getCurrentBlockEnd
                code <- Graph.withUnit loc $ use Graph.code
                return (blockEnd, code)
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                return (starts, code)
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                offsets <- Graph.withUnit loc $ do
                    funs <- use $ Graph.userState . Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo",10), ("bar",58), ("main",91)]
        it "maintains proper function file offsets after adding a function" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                u1 <- mkUUID
                Graph.addNode loc u1 "def aaa" (atXPos $ 1.5 * gapBetweenNodes)
                offsets <- Graph.withUnit loc $ do
                    funs <- use $ Graph.userState . Graph.clsFuns
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
                offsets <- Graph.withUnit loc $ do
                    funs <- use $ Graph.userState . Graph.clsFuns
                    return $ map (\fun -> (fun ^. Graph.funName, fun ^. Graph.funGraph . Graph.fileOffset)) $ Map.elems funs
                return offsets
            offsets `shouldMatchList` [("foo",10), ("main",39)]
        it "maintains proper function file offsets after renaming a function" $ \env -> do
            offsets <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                nodes <- Graph.getNodes loc
                let Just bar = find (\n -> n ^. Node.name == Just "bar") nodes
                Graph.renameNode loc (bar ^. Node.nodeId) "qwerty"
                offsets <- Graph.withUnit loc $ do
                    funs <- use $ Graph.userState . Graph.clsFuns
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
                import Std
                import Foo

                def main:
                    None
                |]
        it "pastes top level function" $ \env -> do
            (nodes, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc multiFunCode
                Graph.pasteText loc [Range 23 23] ["def quux: None"]
                Graph.substituteCode "TestPath" [(46,46,"\n")]
                (,) <$> Graph.getNodes loc <*> Graph.getCode loc
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
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
                let loc = GraphLocation "/TestProject" $ Breadcrumb []
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
                let loc = GraphLocation "/TestProject" $ Breadcrumb []
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
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                clipboard <- Graph.copyText loc [Range 14 36]
                Graph.paste loc (Position.fromTuple (1000, 0)) $ Text.unpack clipboard
                Graph.substituteCodeFromPoints file [TextDiff (Just (Point 4 4, Point 8 4)) "" Nothing]
        it "pastes multiline code from text editor to node editor at the beginning" $
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
                clipboard <- Graph.copyText loc [Range 14 36]
                Graph.paste loc (Position.fromTuple (-1000, 0)) $ Text.unpack clipboard
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
        it "adds def" $
            let initialCode = ""
                expectedCode = [r|
                    def func:
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                node <- Graph.addNode loc u1 "def" def
                liftIO $ node ^. Node.name `shouldBe` Just ""
        it "adds def name" $
            let initialCode = ""
                expectedCode = [r|
                    def foo:
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                node <- Graph.addNode loc u1 "def foo" def
                liftIO $ node ^. Node.name `shouldBe` Just "foo"
        it "adds def with invalid name" $
            let initialCode = ""
                expectedCode = [r|
                    def 4:
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                node <- Graph.addNode loc u1 "def 4" def
                liftIO $ node ^. Node.name `shouldBe` Just "4"
        it "adds def with invalid name and renames it from graph" $
            let initialCode = ""
                expectedCode = [r|
                    def foo:
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "def 4" def
                Graph.renameNode loc u1 "foo"
                node <- Graph.withUnit loc $ runASTOp $ GraphBuilder.buildClassNode u1 ""
                liftIO $ node ^. Node.name `shouldBe` Just "foo"
                return ()
        it "adds invalid def, another node inside and connects to output" $ \env -> do
            evalEmp env $ do
                let initialCode = normalizeLunaCode [r|
                        def main:
                            None
                        |]
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc initialCode
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "def foo" def
                Graph.addNode (loc |>= u1) u2 "4" def
                (_, output) <- Graph.withGraph (loc |>= u1) $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.connect (loc |>= u1) (outPortRef u2 [])
                    (InPortRef' $ inPortRef output [])
                code <- Graph.getCode loc
                liftIO $ normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
                    def foo:
                        number1 = 4
                        number1

                    def main:
                        None
                    |]
        it "uses defined class in main" $
            let initialCode = [r|
                    class Foo:
                        a :: Int
                        b :: Real

                        def baz a : "Hello" + a

                    def main:
                        test = "Hello"
                        None
                    |]
                expectedCode = [r|
                    class Foo:
                        a :: Int
                        b :: Real

                        def baz a : "Hello" + a

                    def main:
                        test = "Hello"
                        Foo.baz
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                Graph.substituteCode file [(102, 102, "\n    Foo.baz")]
        it "uses defined class with list of fields in main" $
            let initialCode = [r|
                    class Foo:
                        a, c, d, e :: Int
                        b :: Real

                        def baz a : "Hello" + a

                    def main:
                        test = "Hello"
                        None
                        |]
                expectedCode = [r|
                    class Foo:
                        a, c, d, e :: Int
                        b :: Real

                        def baz a : "Hello" + a

                    def main:
                        test = "Hello"
                        Foo.baz
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                Graph.substituteCode file [(111, 111, "\n    Foo.baz")]
        it "does not error on incomplete import" $
            let initialCode = [r|
                    import Std.Base
                    import Foo.

                    def main:
                        test = "Hello"
                        None
                    |]
            in specifyCodeChange initialCode initialCode $ \(GraphLocation file _) -> do
                imports <- Graph.getAvailableImports (GraphLocation file def)
                liftIO $ toList imports `shouldMatchList` ["Native", "Std.Base"]
        it "creates and uses invalid top-level function" $
            let initialCode = [r|
                    def main:
                        test = "Hello"
                        None
                    |]
                expectedCode = [r|
                    def foo:
                        number1 = 15
                        None
                    |]
            in specifyCodeChange initialCode expectedCode $
                \(GraphLocation file (Breadcrumb [main])) -> do
                    u1 <- mkUUID
                    let loc = GraphLocation file def
                    Graph.addNode loc u1 "def foo" def
                    names <- fmap (view Node.name) <$> Graph.getNodes loc
                    liftIO $ names `shouldMatchList` [Just "foo", Just "main"]
                    u2 <- mkUUID
                    Graph.addNode (loc |>= u1) u2 "15" def
                    Graph.removeNodes loc [main ^. Breadcrumb.nodeId]

