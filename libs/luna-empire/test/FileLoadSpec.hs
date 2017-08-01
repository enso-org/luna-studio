{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module FileLoadSpec (spec) where

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
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import           Empire.Data.AST                 (SomeASTException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph               as Graph (breadcrumbHierarchy, code, codeMarkers)
import           Empire.Empire                   (CommunicationEnv (..), Empire)
import qualified Luna.Syntax.Text.Parser.Parser  as Parser (ReparsingChange (..), ReparsingStatus (..))
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (Definition))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (Point))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))
import           LunaStudio.Data.Vector2         (Vector2 (..))

import           Empire.Prelude
import           Luna.Prelude                    (normalizeQQ)

import           Test.Hspec                      (Expectation, Spec, around, describe, expectationFailure, it, parallel, shouldBe,
                                                  shouldMatchList, shouldNotBe, shouldSatisfy, shouldStartWith, xit)

import           EmpireUtils

import           Text.RawString.QQ               (r)

import qualified Luna.IR                         as IR

mainCondensed = [r|def main:
    «0»pi = 3.14
    «1»foo = a: b: «4»a + b
    «2»c = 4
    «3»bar = foo 8 c
|]

mainFile = [r|def main:
    «0»pi = 3.14

    «1»foo = a: b: «4»a + b

    «2»c = 4
    «3»bar = foo 8 c
|]

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
    «3»bar = foo 8.0 c
|]

atXPos = ($ def) . (NodeMeta.position . Position.x .~)

specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode act env = do
    let normalize = Text.pack . normalizeQQ . Text.unpack
    actualCode <- evalEmp env $ do
        Library.createLibrary Nothing "TestPath"
        let loc = GraphLocation "TestPath" $ Breadcrumb []
        Graph.loadCode loc $ normalize initialCode
        [main] <- Graph.getNodes loc
        let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
        (nodeIds, toplevel) <- Graph.withGraph loc' $ do
            markers  <- fmap fromIntegral . Map.keys <$> use Graph.codeMarkers
            ids      <- runASTOp $ forM markers $ \i -> (i,) <$> Graph.getNodeIdForMarker i
            toplevel <- uses Graph.breadcrumbHierarchy BH.topLevelIDs
            return (ids, toplevel)
        forM nodeIds $ \(i, Just nodeId) ->
            when (elem nodeId toplevel) $
                Graph.setNodeMeta loc' nodeId $ NodeMeta (Position.fromTuple (0, fromIntegral i*10)) False def
        act loc'
        Text.pack <$> Graph.getCode loc'
    Text.strip actualCode `shouldBe` normalize expectedCode


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "text coordinates translation" $ do
        it "translates points to deltas and back" $ \env -> do
            let code = Text.unlines [ "  "
                                    , "foo   "
                                    , " barbaz"
                                    , ""
                                    , "cwddd   "
                                    , ""
                                    , ""
                                    ]
            Code.pointToDelta (Point 3 2) code `shouldBe` 13
            Code.pointToDelta (Point 0 0) code `shouldBe` 0
            Code.pointToDelta (Point 4 4) code `shouldBe` 23

            Code.deltaToPoint 13 code `shouldBe` (Point 3 2)
            Code.deltaToPoint 0  code `shouldBe` (Point 0 0)
            Code.deltaToPoint 23 code `shouldBe` (Point 4 4)
    describe "code marker removal" $ do
        it "removes markers" $ \env -> do
            let code = Text.unlines [ "def main:"
                                    , "    «0»foo = bar"
                                    , ""
                                    , "    «1»a = x: y: «2»bar + baz"
                                    , "    «3»foobar = bar * baz + foo"
                                    , "    "
                                    ]
                expectedCode = Text.unlines [ "def main:"
                                            , "    foo = bar"
                                            , ""
                                            , "    a = x: y: bar + baz"
                                            , "    foobar = bar * baz + foo"
                                            , "    "
                                            ]
            Code.removeMarkers code `shouldBe` expectedCode
    describe "file loading" $ do
        it "parses unit" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: a + b
                    «2»bar = foo c 6
                    «3»print pi
                    «4»c = 3
                |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                graph <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildGraph
                return graph
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                let Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                pi ^. Node.code `shouldBe` "3.14"
                pi ^. Node.canEnter `shouldBe` False
                let Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                foo ^. Node.code `shouldBe` "a: b: «5»a + b"
                foo ^. Node.canEnter `shouldBe` True
                let Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                bar ^. Node.code `shouldBe` "foo c 6"
                bar ^. Node.canEnter `shouldBe` False
                let Just anon = find (\node -> node ^. Node.name == Nothing) nodes
                anon ^. Node.code `shouldBe` "print pi"
                anon ^. Node.canEnter `shouldBe` False
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                c ^. Node.code `shouldBe` "3"
                c ^. Node.canEnter `shouldBe` False
                connections `shouldMatchList` [
                      (outPortRef (pi ^. Node.nodeId)  [], inPortRef (anon ^. Node.nodeId) [Port.Arg 0])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "does not duplicate nodes on edit" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" 68 68 "3" (Just 69)
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "double modification gives proper value" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" 68 68 "3" (Just 69)
                Graph.substituteCode "TestPath" 68 68 "3" (Just 69)
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                cNode ^. Node.code `shouldBe` "334"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "modifying two expressions give proper values" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" 68 68 "3" (Just 69)
                Graph.substituteCode "TestPath" 88 88 "1" (Just 88)
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                cNode ^. Node.code `shouldBe` "34"
                bar ^. Node.code `shouldBe` "foo 18 c"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "adding an expression works" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" 89 89 "    d = 10\n" (Just 89)
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just d = find (\node -> node ^. Node.name == Just "d") nodes
                d ^. Node.code `shouldBe` "10"
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "unparseable expression does not sabotage whole file" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" 22 26 ")" (Just 26)
                Graph.substituteCode "TestPath" 22 23 "5" (Just 23)
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                    Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                pi ^. Node.code `shouldBe` "5"
                c ^. Node.code `shouldBe` "4"
                bar ^. Node.code `shouldBe` "foo 8 c"
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , (outPortRef (foo ^. Node.nodeId) [], inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "enters lambda written in file" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»foo = a: b: a + b
                    |]
                loc = GraphLocation "TestPath" $ Breadcrumb []
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Just foo <- Graph.withGraph loc' $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.withGraph (loc' |> foo) $ runASTOp $ GraphBuilder.buildGraph
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                nodes `shouldSatisfy` ((== 1) . length)
                connections `shouldSatisfy` ((== 3) . length)
        it "lambda in code can be entered" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»foo = a: a
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Just foo <- Graph.withGraph loc' $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.getGraph $ loc' |> foo
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                nodes `shouldBe` []
                connections `shouldSatisfy` (not . null)
        it "autolayouts nodes on file load" $ \env -> do
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.autolayout loc'
                view Graph.nodes <$> Graph.getGraph loc'
            let positions = map (view (Node.nodeMeta . NodeMeta.position)) nodes
                uniquePositions = Set.size $ Set.fromList positions
            uniquePositions `shouldBe` length nodes
        it "retains node ids on code reload" $ \env -> do
            (prev, new, foo, newFoo) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc testLuna
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Just foo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                previousGraph <- Graph.getGraph (loc' |> foo)
                Graph.substituteCode "TestPath" 65 66 "5" (Just 66)
                Just newFoo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                newGraph <- Graph.getGraph (loc' |> foo)
                return (previousGraph, newGraph, foo, newFoo)
            let Just lala = find (\n -> n ^. Node.name == Just "lala") $ new ^. Graph.nodes
            lala ^. Node.code `shouldBe` "15.0"
            newFoo `shouldBe` foo
            new ^. Graph.inputSidebar `shouldBe` prev ^. Graph.inputSidebar
            new ^. Graph.connections `shouldBe` prev ^. Graph.connections
            new ^. Graph.outputSidebar `shouldBe` prev ^. Graph.outputSidebar
        it "preserves node meta on code reload" $ \env -> do
            meta <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Just foo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                Graph.setNodeMeta loc' foo (NodeMeta (Position.Position (Vector2 15.3 99.2)) True Nothing)
                Graph.substituteCode "TestPath" 63 64 "5" (Just 64)
                Graph.getNodeMeta loc' foo
            meta `shouldBe` Just (NodeMeta (Position.Position (Vector2 15.3 99.2)) True Nothing)
    describe "code spans" $ do
        it "simple example" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 5
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                forM [0..0] $ Graph.markerCodeSpan loc'
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 23)
                    ]
        it "not so simple example" $ \env -> do
            let code = Text.pack $ normalizeQQ $ [r|
                    def main:
                        «0»pi = 5
                        «1»a = 60
                    |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                forM [0..1] $ Graph.markerCodeSpan loc'
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 23)
                    , (28, 37)
                    ]
        it "shows proper expressions ranges" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                forM [0..3] $ Graph.markerCodeSpan loc'
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 26)
                    , (31, 54)
                    , (59, 67)
                    , (72, 88)
                    ]
        it "updateCodeSpan does not break anything" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.withGraph loc' $ do
                    runASTOp $ do
                        Just nodeSeq <- GraphBuilder.getNodeSeq
                        Graph.updateCodeSpan nodeSeq
                forM [0..3] $ Graph.markerCodeSpan loc'
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 26)
                    , (31, 54)
                    , (59, 67)
                    , (72, 88)
                    ]
        it "assigns nodeids to marked expressions" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.withGraph loc' $ runASTOp $ forM [0..3] Graph.getNodeIdForMarker
            withResult res $ \ids -> do
                ids `shouldSatisfy` (all isJust)
        it "autolayouts nested nodes on file load" $ \env -> do
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
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.autolayout loc'
                Just foo <- Graph.withGraph loc' $ runASTOp $ Graph.getNodeIdForMarker 1
                view Graph.nodes <$> Graph.getGraph (loc' |> foo)
            let positions = map (view (Node.nodeMeta . NodeMeta.position)) nodes
                uniquePositions = Set.size $ Set.fromList positions
            uniquePositions `shouldBe` length nodes
    describe "code modifications by graph operations" $ do
        it "adds one node to code" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                [main] <- Graph.getNodes (GraphLocation "TestFile" (Breadcrumb []))
                let loc' = GraphLocation "TestFile" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.addNode top u1 "4" (atXPos (-20.0))
                Graph.getCode top
            code `shouldBe` "def main:\n    number1 = 4\n    None"
        it "adds one node and updates it" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                Graph.addNode top u1 "4" (atXPos (-10))
                Graph.markerCodeSpan top 0
                Graph.setNodeExpression top u1 "5"
                Graph.getCode top
            code `shouldBe` "def main:\n    number1 = 5\n    None"
        it "disconnect updates code at proper range" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                [Just c, Just bar] <- Graph.withGraph loc $ runASTOp $ mapM (Graph.getNodeIdForMarker) [2,3]
                Graph.disconnect loc (inPortRef bar [Port.Arg 1])
        it "disconnect/connect updates code at proper range" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 pi
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                [Just pi, Just bar] <- Graph.withGraph loc $ runASTOp $ mapM (Graph.getNodeIdForMarker) [0,3]
                Graph.disconnect loc (inPortRef bar [Port.Arg 1])
                Graph.connect loc (outPortRef pi []) (InPortRef' $ inPortRef bar [Port.Arg 1])
        it "adds one node to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    number1 = 4
                |]
            in specifyCodeChange mainCondensed expectedCode $ \top -> do
                u1 <- mkUUID
                Graph.addNode top u1 "4" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "adds one node to the beginning of the file via node editor" $ let
            expectedCode = [r|
                def main:
                    number1 = 4
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \top -> do
                u1 <- mkUUID
                Graph.addNode top u1 "4" (NodeMeta (Position.fromTuple (-10, 0)) False def)
        it "adds one named node to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    someNode = 123456789
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "someNode = 123456789" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "trims whitespace when adding node via node editor" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    number1 = 1
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "           1" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "preserves original whitespace inside expression when adding node" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    lambda1 = a:   b:   a   *  b
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "a:   b:   a   *  b" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "adds lambda to existing file via node editor" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    lambda1 = x: x
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "x: x" (NodeMeta (Position.fromTuple (10, 50)) False def)
        it "adds node via node editor and removes it" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "x :   x" (NodeMeta (Position.fromTuple (10, 25)) False def)
                Graph.removeNodes loc [u1]
        it "removes last node form a file" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                    None
                |]
            expectedCode = [r|
                def main:
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just id <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.removeNodes loc [id]
        it "removes all nodes from a file, then adds some" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                    None
                |]
            expectedCode = [r|
                def main:
                    foo = 3 + 5
                    bar = 20 + 30
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                ids <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..3]
                Graph.removeNodes loc (fromJust <$> ids)
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "foo = 3 + 5"   (atXPos 20.0)
                Graph.addNode loc u2 "bar = 20 + 30" (atXPos 30.0)

        it "adds and removes nodes inside a lambda" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: d = 8
                                a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just id <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |> id
                u1 <- mkUUID
                Graph.addNode loc' u1 "x = 2 + 3 +    5" (atXPos 0)
                u2 <- mkUUID
                Graph.addNode loc' u2 "d = 8" (atXPos (-10.0))
                Graph.removeNodes loc' [u1]
        it "updates code span after editing an expression" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 123456789
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1     <- mkUUID
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.setNodeExpression loc c "123456789"
        it "renames unused node in code" $ let
            expectedCode = [r|
                def main:
                    ddd = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc pi "ddd"
        it "renames used node in code" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    ddd = 4
                    bar = foo 8 ddd
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.renameNode loc c "ddd"
        it "adds one node to existing file and updates it" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    number1 = 5
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (10, 50)) False def)
                Graph.setNodeExpression loc u1 "5"
        it "adds multiple nodes" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                    sum1 = (foo +  baz)
                    add1 = add here
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "(foo +  baz)" (NodeMeta (Position.fromTuple (10, 60)) False def)
                u2 <- mkUUID
                Graph.addNode loc u1 "add here" (NodeMeta (Position.fromTuple (10, 50)) False def)
        it "combines adding and renaming nodes" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b = a .    succ
                |]
            expectedCode = [r|
                def main:
                    foobar = 20
                    b = foobar .    succ
                    bar = foobar .   div   foobar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just a <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc a "foo"
                u1 <- mkUUID
                Graph.addNode loc u1 "bar = foo .   div   foo" (NodeMeta (Position.fromTuple (0, 30)) False def)
                Graph.renameNode loc a "foobar"
        it "renames node under operator" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b =   a + a
                |]
            expectedCode = [r|
                def main:
                    foobar = 20
                    b =   foobar + foobar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just a <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc a "foobar"
        it "connects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b =   bar
                |]
            expectedCode = [r|
                def main:
                    a = 20
                    foo = 30
                    b =   foo . bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b, Just foo] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [2, 1]
                Graph.connect loc (outPortRef foo []) (InPortRef' $ inPortRef b [Port.Self])
        it "reconnects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»foo = 30
                    «2»b = a  .   bar
                |]
            expectedCode = [r|
                def main:
                    a = 20
                    foo = 30
                    b = foo  .   bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b, Just foo] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [2, 1]
                Graph.connect loc (outPortRef foo []) (InPortRef' $ inPortRef b [Port.Self])
        it "disconnects self port retaining formatting" $ let
            initialCode = [r|
                def main:
                    «0»a = 20
                    «1»b =   a  .   bar
                |]
            expectedCode = [r|
                def main:
                    a = 20
                    b =   bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [1]
                Graph.disconnect loc (inPortRef b [Port.Self])
        it "connects and then disconnects self multiple times" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = succ
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self])
                Graph.disconnect loc (inPortRef b [Port.Self])
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self])
                Graph.disconnect loc (inPortRef b [Port.Self])
        it "connects to application port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = succ baz _ node1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Arg 2])
        it "connects to a deep self port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = Empty
                    «1»b = prepend 10 . prepend 20
                |]
            expectedCode = [r|
                def main:
                    node1 = Empty
                    b = node1 . prepend 10 . prepend 20
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self, Port.Self])
        it "connects to a deep application port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = Empty
                    «1»b = node1 . prepend . prepend 20
                |]
            expectedCode = [r|
                def main:
                    node1 = Empty
                    b = node1 . prepend node1 . prepend 20
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.connect loc (outPortRef a []) (InPortRef' $ inPortRef b [Port.Self, Port.Arg 0])
        it "disconnects an application port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz _ node1
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = succ baz
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just b <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Graph.disconnect loc $ inPortRef b [Port.Arg 2]
        it "disconnects a self port behind an application" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz (node1 . prepend 1) node1
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = succ baz (prepend 1) node1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just b <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Graph.disconnect loc $ inPortRef b [Port.Arg 1, Port.Self]
        it "disconnects a deep self port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = foo . prepend 10 . prepend 20
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = prepend 10 . prepend 20
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just b <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Graph.disconnect loc $ inPortRef b [Port.Self, Port.Self]
        it "disconnects a deep application port" $ let
            initialCode = [r|
                def main:
                    «0»node1 = foo
                    «1»b = succ baz (Empty . prepend node1 foo) node1
                |]
            expectedCode = [r|
                def main:
                    node1 = foo
                    b = succ baz (Empty . prepend _ foo) node1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just b <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Graph.disconnect loc $ inPortRef b [Port.Arg 1, Port.Arg 0]
        it "connects to application port multiple times" $ let
            initialCode = [r|
                def main:
                    «0»a  = foo
                    «1»bb = bar
                    «2»ccc = baz
                    «3»dddd = spam bb
                |]
            expectedCode = [r|
                def main:
                    a  = foo
                    bb = bar
                    ccc = baz
                    dddd = spam a bb ccc
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just a, Just bb, Just ccc, Just dddd] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..3]
                Graph.connect loc (outPortRef ccc []) (InPortRef' $ inPortRef dddd [Port.Arg 2])
                Graph.connect loc (outPortRef a   []) (InPortRef' $ inPortRef dddd [Port.Arg 0])
                Graph.connect loc (outPortRef bb  []) (InPortRef' $ inPortRef dddd [Port.Arg 1])

        it "applies operators at first argument" $ let
            initialCode = [r|
                def main:
                    «0»aa = foo
                    «1»b  = +
                |]
            expectedCode = [r|
                def main:
                    aa = foo
                    b  = aa +
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just aa, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..1]
                Graph.connect loc (outPortRef aa []) (InPortRef' $ inPortRef b [Port.Arg 0])

        it "applies operators at both arguments" $ let
            initialCode = [r|
                def main:
                    «0»aa  = foo
                    «1»bar = foobar
                    «2»c   = +
                |]
            expectedCode = [r|
                def main:
                    aa  = foo
                    bar = foobar
                    c   = aa + bar
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just aa, Just bar, Just c] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..2]
                Graph.connect loc (outPortRef bar []) (InPortRef' $ inPortRef c [Port.Arg 1])
                Graph.connect loc (outPortRef aa [])  (InPortRef' $ inPortRef c [Port.Arg 0])

        it "applies operators at second argument only" $ let
            initialCode = [r|
                def main:
                    «0»aa = foo
                    «1»b  = +
                |]
            expectedCode = [r|
                def main:
                    aa = foo
                    b  = _ + aa
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just aa, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..1]
                Graph.connect loc (outPortRef aa []) (InPortRef' $ inPortRef b [Port.Arg 1])

        it "applies operators at the first argument when the second is already applied " $ let
            initialCode = [r|
                def main:
                    «0»aa = foo
                    «1»b  = + buzz
                |]
            expectedCode = [r|
                def main:
                    aa = foo
                    b  = aa + buzz
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just aa, Just b] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0..1]
                Graph.connect loc (outPortRef aa []) (InPortRef' $ inPortRef b [Port.Arg 0])

        it "updates code after connecting lambda output" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: baz = bar a b
                                a + b
                                baz
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just foo    <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                (_, output) <- Graph.withGraph (loc |> foo) $ runASTOp $ GraphBuilder.getEdgePortMapping
                u1 <- mkUUID
                Graph.addNode (loc |> foo) u1 "baz = bar a b" $ atXPos (-10.0)
                Graph.connect (loc |> foo) (outPortRef u1 []) (InPortRef' $ inPortRef output [])

        it "updates code after disconnecting lambda output" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                                None
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                (_, output) <- Graph.withGraph (loc |> foo) $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.disconnect (loc |> foo) (inPortRef output [])
        it "preserves code after connecting & disconnecting lambda output" $ let
            code = [r|
                def main a:
                    None
                |]
            in specifyCodeChange code code $ \loc -> do
                (input, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.connect    loc (outPortRef input [Port.Projection 0]) (InPortRef' $ inPortRef output [])
                Graph.disconnect loc (inPortRef output [])
        it "handles collapsing nodes into functions" $ let
            initialCode = [r|
                def main:
                    «0»foo = bar
                    «1»baz = buzz foo
                    «2»spam = eggs baz
                    «3»a = baz + 1
                |]
            expectedCode = [r|
                def main:
                    foo = bar
                    def func1 foo:
                        baz = buzz foo
                        spam = eggs baz
                        baz
                    baz = func1 foo
                    a = baz + 1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just baz, Just spam] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [1, 2]
                Graph.collapseToFunction loc [baz, spam]
        it "handles collapsing nodes into functions with multiple uses of an argument" $ let
            initialCode = [r|
                def main:
                    «3»uri = "https://min-api.cryptocompare.com/data/price?fsym="
                    «5»crypto = "BTC"
                    «6»withCrypto = uri + crypto
                    «7»fiat = "USD"
                    «8»fullUri = withCrypto + "&tsyms=" + fiat
                    «4»response = Http.getJSON fullUri
                    «9»result = response . lookupReal fiat
                    «10»foo = id result
                |]
            expectedCode = [r|
                def main:
                    crypto = "BTC"
                    fiat = "USD"
                    def func1 crypto fiat:
                        uri = "https://min-api.cryptocompare.com/data/price?fsym="
                        withCrypto = uri + crypto
                        fullUri = withCrypto + "&tsyms=" + fiat
                        response = Http.getJSON fullUri
                        result = response . lookupReal fiat
                        result
                    result = func1 crypto fiat
                    foo = id result
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just uri, Just withCrypto, Just fullUri, Just response, Just result] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [3, 6, 8, 4, 9]
                Graph.collapseToFunction loc [uri, withCrypto, fullUri, response, result]
