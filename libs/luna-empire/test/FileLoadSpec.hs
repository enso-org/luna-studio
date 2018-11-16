{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}

module FileLoadSpec (spec) where

import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TChan    (tryReadTChan)
import           Control.Lens                    ((^..), prism)
import           Control.Monad                   (forM)
import           Control.Monad.Loops             (unfoldM)
import qualified Data.Graph.Data.Component.Set   as MutableSet
import qualified Data.Graph.Store                as Store
import           Data.List                       (find, maximum)
import           Data.Maybe                      (fromJust)
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Builder           as ASTBuilder
import qualified Empire.ASTOps.Modify            as ASTModify
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.Code            as Code
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import qualified Empire.Commands.Library         as Library
import qualified Empire.Commands.Typecheck       as Typecheck
import qualified Empire.Data.Graph               as Graph (CommandState(..),
                                                           clsClass, code,
                                                           defaultPMState,
                                                           nodeCache, userState)
import           Empire.Empire                   (CommunicationEnv (..), InterpreterEnv(..))
import qualified Luna.Package.Structure.Generate as Package
import           LunaStudio.API.AsyncUpdate      (AsyncUpdate(ResultUpdate))
import qualified LunaStudio.API.Graph.NodeResultUpdate as NodeResult
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (Definition))
import qualified LunaStudio.Data.Connection      as Connection
import           LunaStudio.Data.Connection      (Connection (..))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..), (|>|), (|>=))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import qualified LunaStudio.Data.NodeMeta        as NodeMeta
import           LunaStudio.Data.Point           (Point (Point))
import qualified LunaStudio.Data.Port            as Port
import qualified LunaStudio.Data.PortDefault     as PortDefault
import           LunaStudio.Data.PortRef         (AnyPortRef (..))
import qualified LunaStudio.Data.PortRef         as PortRef
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.Range           (Range (..))
import           LunaStudio.Data.TextDiff        (TextDiff (..))
import           LunaStudio.Data.NodeValue
import           LunaStudio.Data.Vector2               (Vector2 (..))
import           LunaStudio.Data.Visualization         (VisualizationValue (Value))
import qualified LunaStudio.Data.LabeledTree           as LabeledTree
import           System.FilePath                       ((</>))
import qualified System.IO.Temp                        as Temp

import           Empire.Prelude                        hiding (fromJust, minimum, maximum, pi)

import           Test.Hspec                            (Selector, Spec, around, describe, it, parallel, shouldBe,
                                                        shouldMatchList, shouldSatisfy, shouldThrow)

import           EmpireUtils

import           Text.RawString.QQ                     (r)


mainCondensed :: Text
mainCondensed = [r|def main:
    «0»pi = 3.14
    «1»foo = a: b: «4»a + b
    «2»c = 4
    «3»bar = foo 8 c
|]

mainFile :: Text
mainFile = [r|def main:
    «0»pi = 3.14

    «1»foo = a: b: «4»a + b

    «2»c = 4
    «3»bar = foo 8 c
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
    «3»bar = foo 8.0 c
|]

testLuna' :: Text
testLuna' = [r|def main:
    «1»foo = a: b:
        «7»n = a + 5
        «8»m = b - 2
        «11»m + n
|]

atXPos :: Double -> NodeMeta
atXPos = ($ def) . (NodeMeta.position . Position.x .~)


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "text coordinates translation" $ do
        it "translates points to deltas and back" $ \_ -> do
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
    describe "removes meta" $ do
        it "removes meta" $ \_ -> do
            let code = [r|def main:
    «0»pi = 3.14
    None

### META {"metas":[]}
|]
                expectedCode = [r|def main:
    «0»pi = 3.14
    None
|]
            Graph.stripMetadata code `shouldBe` expectedCode
    describe "code marker removal" $ do
        it "removes markers" $ \_ -> do
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
            let code = normalizeLunaCode $ [r|
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
            withResult res $ \(Graph.Graph nodes connections i _ _ _) -> do
                let Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                pi ^. Node.code `shouldBe` "3.14"
                pi ^. Node.canEnter `shouldBe` False
                let Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                foo ^. Node.code `shouldBe` "a: b: a + b"
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
                i ^? _Just . Node.isDef `shouldBe` Just True
                connections `shouldMatchList` [
                      Connection (outPortRef (pi ^. Node.nodeId)  []) (inPortRef (anon ^. Node.nodeId) [Port.Arg 0])
                    , Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "parses unit with empty line before imports" $ \env -> do
            let code = Text.pack $ [r|

import Std.Base

def main:
    «0»pi = 3.14
                |]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                graph <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildGraph
                return graph
            withResult res $ \(Graph.Graph nodes _ _ _ _ _) -> do
                let Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                pi ^. Node.code `shouldBe` "3.14"
                pi ^. Node.canEnter `shouldBe` False
        it "does not duplicate nodes on edit" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" [(71, 71, "3")]
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                connections `shouldMatchList` [
                      Connection (outPortRef (cNode ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , Connection (outPortRef (foo   ^. Node.nodeId) []) (inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "double modification gives proper value" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" [(71, 71, "3")]
                Graph.substituteCode "TestPath" [(71, 71, "3")]
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                cNode ^. Node.code `shouldBe` "334"
                connections `shouldMatchList` [
                      Connection (outPortRef (cNode ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , Connection (outPortRef (foo   ^. Node.nodeId) []) (inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "modifying two expressions give proper values" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainFile
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" [(71, 71, "3")]
                Graph.substituteCode "TestPath" [(91, 91, "1")]
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                cNode ^. Node.code `shouldBe` "34"
                bar ^. Node.code `shouldBe` "foo 18 c"
                connections `shouldMatchList` [
                      Connection (outPortRef (cNode ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , Connection (outPortRef (foo   ^. Node.nodeId) []) (inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "adding an expression works" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" [(92, 92, "    d = 10\n")]
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                    Just d = find (\node -> node ^. Node.name == Just "d") nodes
                d ^. Node.code `shouldBe` "10"
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                connections `shouldMatchList` [
                      Connection (outPortRef (c   ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Head])
                    ]
        it "adding None at the end works" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    «0»hello = "Hello"
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    hello = "Hello"
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 19 3), (Point 19 3))) "\n    " (Just (Point 4 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 4 4), (Point 4 4))) "No" (Just (Point 6 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 6 4), (Point 6 4))) "ne" (Just (Point 8 4))]
        it "removing None at the end works" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    «0»hello = "Hello"
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    hello = "Hello"
                |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 21 3), (Point 21 3))) "\n    " (Just (Point 4 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 4 4), (Point 4 4))) "No" (Just (Point 6 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 6 4), (Point 6 4))) "ne" (Just (Point 8 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 7 4), (Point 8 4))) "" (Just (Point 7 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 6 4), (Point 7 4))) "" (Just (Point 6 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 5 4), (Point 6 4))) "" (Just (Point 5 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 4 4), (Point 5 4))) "" (Just (Point 4 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 0 4), (Point 4 4))) "" (Just (Point 0 4))]
                Graph.substituteCodeFromPoints file [TextDiff (Just ((Point 21 3), (Point 0 4))) "" (Just (Point 21 3))]
        it "unparseable expression does not sabotage whole file" $ \env -> do
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc mainCondensed
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.substituteCode "TestPath" [(25, 29, ")")]
                Graph.substituteCode "TestPath" [(25, 26, "5")]
                Graph.getGraph loc'
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                    Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                    Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                    Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                pi ^. Node.code `shouldBe` "5"
                c ^. Node.code `shouldBe` "4"
                bar ^. Node.code `shouldBe` "foo 8 c"
                connections `shouldMatchList` [
                      Connection (outPortRef (c   ^. Node.nodeId) []) (inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    , Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef (bar  ^. Node.nodeId) [Port.Head])
                    ]
        it "enters lambda written in file" $ \env -> do
            let code = normalizeLunaCode $ [r|
                    def main:
                        «0»foo = a: b: a + b
                    |]
                loc = GraphLocation "TestPath" $ Breadcrumb []
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = loc |>= main ^. Node.nodeId
                Just foo <- Graph.withGraph loc' $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.withGraph (loc' |>| foo) $ runASTOp $ GraphBuilder.buildGraph
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                nodes `shouldSatisfy` ((== 1) . length)
                connections `shouldSatisfy` ((== 3) . length)
        it "lambda in code can be entered" $ \env -> do
            let code = normalizeLunaCode $ [r|
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
                Graph.getGraph $ loc' |>| foo
            withResult res $ \(Graph.Graph nodes connections _ _ _ _) -> do
                nodes `shouldBe` mempty
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
                previousGraph <- Graph.getGraph (loc' |>| foo)
                Graph.substituteCode "TestPath" [(69, 70, "5")]
                Just newFoo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                newGraph <- Graph.getGraph (loc' |>| foo)
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
                Graph.substituteCode "TestPath" [(63, 64, "5")]
                a <- Graph.getNodeMeta loc' foo
                return a
            meta `shouldBe` Just (NodeMeta (Position.Position (Vector2 15.3 99.2)) True Nothing)
        it "changing order of ports twice does nothing" $ \env -> do
            -- [MM]: don't know why some nodes have empty code only in `before` so this test fails
            (before, after) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc testLuna'
                [main] <- Graph.getNodes loc
                let loc' = loc |>= main ^. Node.nodeId
                Just foo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                before@(Graph.Graph _ _ (Just input) _ _ _) <- Graph.getGraph $ loc' |>| foo
                Graph.movePort (loc' |>| foo) (outPortRef (input ^. Node.nodeId) [Port.Projection 0]) 1
                Graph.movePort (loc' |>| foo) (outPortRef (input ^. Node.nodeId) [Port.Projection 0]) 1
                after <- Graph.getGraph $ loc' |>| foo
                return (before, after)
            before `shouldBe` after
        it "changing order of ports changes code" $ \env -> do
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc testLuna
                [main] <- Graph.getNodes loc
                let loc' = loc |>= main ^. Node.nodeId
                Just foo <- Graph.withGraph loc' $ runASTOp (Graph.getNodeIdForMarker 1)
                (Graph.Graph _ _ (Just input) _ _ _) <- Graph.getGraph $ loc' |>| foo
                Graph.movePort (loc' |>| foo) (outPortRef (input ^. Node.nodeId) [Port.Projection 0]) 1
                code <- Graph.withUnit loc $ use Graph.code
                return code
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
            «13»def main:
                «0»pi = 3.14
                «1»foo = b: a:
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
    describe "code spans" $ do
        it "simple example" $ \env -> do
            let code = normalizeLunaCode $ [r|
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
                      (17, 26)
                    ]
        it "not so simple example" $ \env -> do
            let code = normalizeLunaCode $ [r|
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
                      (17, 26)
                    , (31, 40)
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
                      (17, 29)
                    , (34, 57)
                    , (62, 70)
                    , (75, 91)
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
                        nodeSeq <- ASTRead.getCurrentBody
                        Graph.updateCodeSpan nodeSeq
                forM [0..3] $ Graph.markerCodeSpan loc'
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (17, 29)
                    , (34, 57)
                    , (62, 70)
                    , (75, 91)
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
            nodes <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Graph.autolayout loc'
                Just foo <- Graph.withGraph loc' $ runASTOp $ Graph.getNodeIdForMarker 1
                view Graph.nodes <$> Graph.getGraph (loc' |>| foo)
            let positions = map (view (Node.nodeMeta . NodeMeta.position)) nodes
                uniquePositions = Set.size $ Set.fromList positions
            uniquePositions `shouldBe` length nodes
    describe "code modifications by graph operations" $ do
        it "adds one node to code" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                Graph.addNode top u1 "4" (atXPos (-20.0))
                Graph.getCode top
            code `shouldBe` "def main:\n    number1 = 4\n    None"
        it "adds one node and updates it" $ \env -> do
            u1 <- mkUUID
            code <- evalEmp env $ do
                Graph.addNode top u1 "4" (atXPos (-10))
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
                [Just bar] <- Graph.withGraph loc $ runASTOp $ mapM (Graph.getNodeIdForMarker) [3]
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
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (0, 5)) False def)
        it "adds one node to the beginning of the file via node editor" $ let
            expectedCode = [r|
                def main:
                    number1 = 4
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (-10, 0)) False def)
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
                Just nodeId <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.removeNodes loc [nodeId]
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
                Graph.removeNodes loc (unsafeFromJust <$> ids)
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "foo = 3 + 5"   (atXPos 20.0)
                Graph.addNode loc u2 "bar = 20 + 30" (atXPos 30.0)
        it "adds and removes nodes inside a lambda" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: d = 8
                                sum1 = a + b
                                sum1
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just nodeId <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| nodeId
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
        it "renames used node in code to pattern" $ let
            initialCode = [r|
                def main:
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
                def main:
                    Just a = 4
                    bar = foo 7 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.renameNode loc c "Just a"
                Just bar <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 3
                Graph.setNodeExpression loc bar "foo 7 c"
        it "renames used node in code to pattern with already used var name" $ let
            initialCode = [r|
                def main:
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
                def main:
                    (b,c) = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.renameNode loc c "(b,c)"
                succs <- Graph.withGraph loc $ runASTOp $ do
                    var   <- ASTRead.getASTVar c
                    vars  <- ASTRead.dumpPatternVars var
                    a     <- mapM (getLayer @IRSuccs) vars
                    mapM MutableSet.size a
                liftIO (maximum succs `shouldBe` 2) -- two uses of c
        it "renames used node in code to number" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    5 = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.renameNode loc c "5"
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
                Graph.addNode loc u2 "add here" (NodeMeta (Position.fromTuple (10, 50)) False def)
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
                (_, output) <- Graph.withGraph (loc |>| foo) $ runASTOp $ GraphBuilder.getEdgePortMapping
                u1 <- mkUUID
                Graph.addNode (loc |>| foo) u1 "baz = bar a b" $ atXPos (-10.0)
                Graph.connect (loc |>| foo) (outPortRef u1 []) (InPortRef' $ inPortRef output [])

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
                (_, output) <- Graph.withGraph (loc |>| foo) $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.disconnect (loc |>| foo) (inPortRef output [])
        it "updates literal node" $ let
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 100000000
                    baz = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                Graph.setPortDefault loc (inPortRef c []) (Just $ PortDefault.Constant (PortDefault.IntValue 100000000))
                Just bar <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 3
                Graph.renameNode loc bar "baz"
        it "updates literal real node" $ let
            expectedCode = [r|
                def main:
                    pi = 2.71828
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 2.71828))
        it "updates literal huge real node" $ let
            expectedCode = [r|
                def main:
                    pi = 5102342220534060.0
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 121.51023535519526))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102352295050615e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102350925977542e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102353663122158e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.10234662807987e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.10234222053406e15))
        it "updates literal really small real node" $ let
            expectedCode = [r|
                def main:
                    pi = 0.00000000000000510234222053406
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 121.51023535519526))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102352295050615e-15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102350925977542e-15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102353663122158e-15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.10234662807987e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.10234222053406e-15))
        it "updates literal huge negative real node" $ let
            expectedCode = [r|
                def main:
                    pi = -5102342220534060.0
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 121.51023535519526))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue -5.102352295050615e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue -5.102350925977542e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.102353663122158e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 5.10234662807987e15))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue -5.10234222053406e15))
        it "updates literal negative real node" $ let
            expectedCode = [r|
                def main:
                    pi = -2.71828
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange mainCondensed expectedCode $ \loc -> do
                Just pi <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue 2.71828))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue -2.7))
                Graph.setPortDefault loc (inPortRef pi []) (Just $ PortDefault.Constant (PortDefault.RealValue -2.71828))
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
                    None
                |]
            expectedCode = [r|
                def func1 foo:
                    baz = buzz foo
                    spam = eggs baz
                    baz

                def main:
                    foo = bar
                    baz = func1 foo
                    a = baz + 1
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just baz, Just spam] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [1, 2]
                Graph.collapseToFunction loc [baz, spam]
        it "handles collapsing all nodes into a function" $ let
            initialCode = [r|
                def main:
                    «0»foo = "AAA"
                    «1»baz = "BBB"
                    «2»spam = eggs foo baz
                    None
                |]
            expectedCode = [r|
                def func1:
                    foo = "AAA"
                    baz = "BBB"
                    spam = eggs foo baz
                    spam

                def main:
                    spam = func1
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1, 2]
                Graph.collapseToFunction loc $ map fromJust nodes
        it "handles collapsing nodes connected to input" $ let
            initialCode = [r|
                def main a:
                    «0»sum1 = a + 2
                    None
                |]
            expectedCode = [r|
                def func1 a:
                    sum1 = a + 2
                    sum1

                def main a:
                    sum1 = func1 a
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just node <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.collapseToFunction loc [node]
        it "handles collapsing pattern-match outputs" $ let
            initialCode = [r|
                def main:
                    «1»a = (1, 2)
                    «0»(a1, b1) = a
                    None
                |]
            expectedCode = [r|
                def func1:
                    a = (1, 2)
                    (a1, b1) = a
                    (a1, b1)

                def main:
                    (a1, b1) = func1
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0, 1]
                Graph.collapseToFunction loc $ map fromJust nodes
        it "handles collapsing anonymous nodes into functions" $ let
            initialCode = [r|
                def main:
                    «0»5
                |]
            expectedCode = [r|
                def func1:
                    5

                def main:
                    func1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just five <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.collapseToFunction loc [five]
        it "handles collapsing nodes with pattern matching into functions" $ let
            initialCode = [r|
                def main:
                    «0»foo = bar
                    «1»(a, b) = (1, 2)
                    «2»baz = a + b
                    «3»c = baz + 1
                |]
            expectedCode = [r|
                def func1:
                    (a, b) = (1, 2)
                    baz = a + b
                    baz

                def main:
                    foo = bar
                    baz = func1
                    c = baz + 1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.getNodes loc
                let Just ab  = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "(a, b)") nodes
                    Just baz = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "baz") nodes
                Graph.collapseToFunction loc [ab, baz]
        it "handles collapsing nodes to proper position" $ let
            initialCode = [r|
                def main:
                    «0»foo = bar
                    «1»(a, b) = (1, 2)
                    «2»baz = a + b
                    «3»c = baz + 1
                |]
            expectedCode = [r|
                def func1:
                    (a, b) = (1, 2)
                    baz = a + b
                    baz

                def main:
                    foo = bar
                    baz = func1
                    c = baz + 1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.getNodes loc
                let Just ab  = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "(a, b)") nodes
                    Just baz = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "baz") nodes
                Just bazMeta <- Graph.getNodeMeta loc baz
                Graph.collapseToFunction loc [ab, baz]
                nodesAfter <- Graph.getNodes loc
                let Just bazAfter = view Node.nodeId <$> find (\n -> n ^. Node.name == Just "baz") nodesAfter
                Just newBazMeta <- Graph.getNodeMeta loc bazAfter
                liftIO $ newBazMeta ^. NodeMeta.position `shouldBe` bazMeta ^. NodeMeta.position
        it "handles collapsing nodes into functions two times" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def func1:
                    (a, b) = (1, 2)
                    sum1 = a + 1
                    sum1

                def func2:
                    c = 4
                    sum2 = c + 2
                    sum2

                def main:
                    sum1 = func1
                    sum2 = func2
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "(a, b) = (1, 2)" def
                Graph.addNode loc u2 "a + 1" def
                u3 <- mkUUID
                u4 <- mkUUID
                Graph.addNode loc u3 "c = 4" def
                Graph.addNode loc u4 "c + 2" def
                Graph.collapseToFunction loc [u1, u2]
                Graph.collapseToFunction loc [u3, u4]
        it "handles collapsing nodes into functions without use of an argument" $ let
            initialCode = [r|
                def main:
                    «0»foo = bar
                    «1»baz = buzz foo
                    «2»spam = eggs baz
                    «3»a = 3 + 1
                |]
            expectedCode = [r|
                def func1 foo:
                    baz = buzz foo
                    spam = eggs baz
                    spam

                def main:
                    foo = bar
                    spam = func1 foo
                    a = 3 + 1
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
                def func1 crypto fiat:
                    uri = "https://min-api.cryptocompare.com/data/price?fsym="
                    withCrypto = uri + crypto
                    fullUri = withCrypto + "&tsyms=" + fiat
                    response = Http.getJSON fullUri
                    result = response . lookupReal fiat
                    result

                def main:
                    crypto = "BTC"
                    fiat = "USD"
                    result = func1 crypto fiat
                    foo = id result
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just uri, Just withCrypto, Just fullUri, Just response, Just result] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [3, 6, 8, 4, 9]
                Graph.collapseToFunction loc [uri, withCrypto, fullUri, response, result]
        it "shows proper names of functions in crypto" $ let
            initialCode = [r|
                import Std.HTTP
                import Std.Time

                «51»def getCurrencyPrice crypto fiat:
                    «40»uri = "https://min-api.cryptocompare.com/data/price"
                    «41»request = Http.get uri
                    «42»withFsym = request . setParam "fsym" (Just crypto)
                    «43»withTsyms = withFsym . setParam "tsyms" (Just fiat)
                    «44»response = withTsyms . perform . json
                    «46»price = response . getReal fiat
                    price

                «52»def main:
                    «47»fiat = "USD"
                    «48»crypto = "BTC"
                    «50»prices = every 1.seconds (getCurrencyPrice crypto fiat)
                    None

                ### META {"metas":[{"marker":47,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":3872}}}},{"marker":48,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":208,"_vector2_x":3872}}}},{"marker":50,"meta":{"_displayResult":true,"_selectedVisualizer":["echarts: plot","echarts/plot.html"],"_position":{"fromPosition":{"_vector2_y":128,"_vector2_x":4208}}}},{"marker":40,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":0}}}},{"marker":41,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":176}}}},{"marker":42,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":352}}}},{"marker":43,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":528}}}},{"marker":44,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":672}}}},{"marker":46,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":0,"_vector2_x":1056}}}}]}
                |]
            expectedCode = [r|
                import Std.HTTP
                import Std.Time

                def getCurrencyPrice crypto fiat:
                    uri = "https://min-api.cryptocompare.com/data/price"
                    request = Http.get uri
                    withFsym = request . setParam "fsym" (Just crypto)
                    withTsyms = withFsym . setParam "tsyms" (Just fiat)
                    response = withTsyms . perform . json
                    price = response . getReal fiat
                    price

                def main:
                    fiat = "USD"
                    crypto = "BTC"
                    prices = every 1.seconds (getCurrencyPrice crypto fiat)
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                funs <- Graph.getNodes (GraphLocation file def)
                liftIO $ map (view Node.name) funs `shouldMatchList` [Just "getCurrencyPrice", Just "main"]
        it "sorts arguments by position when collapsing to function" $ let
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
                def func1 fiat crypto:
                    uri = "https://min-api.cryptocompare.com/data/price?fsym="
                    withCrypto = uri + crypto
                    fullUri = withCrypto + "&tsyms=" + fiat
                    response = Http.getJSON fullUri
                    result = response . lookupReal fiat
                    result

                def main:
                    crypto = "BTC"
                    fiat = "USD"
                    result = func1 fiat crypto
                    foo = id result
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just uri, Just withCrypto, Just fullUri, Just response, Just result, Just fiat, Just crypto] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [3, 6, 8, 4, 9, 7, 5]
                Graph.setNodeMeta loc fiat (atXPos (-1000))
                Graph.setNodeMeta loc crypto (atXPos 1000)
                Graph.collapseToFunction loc [uri, withCrypto, fullUri, response, result]
        it "adds arguments in toplevel defs and adds a node" $ let
            initialCode = [r|
                def main:
                    foo bar
                    baz
                |]
            expectedCode = [r|
                def main a c b:
                    foo bar
                    baz1 = baz
                    foo1 = foo a b c
                    baz1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                (input, _) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPort loc (outPortRef input [Port.Projection 0])
                Graph.addPort loc (outPortRef input [Port.Projection 1])
                Graph.addPort loc (outPortRef input [Port.Projection 1])
                u1 <- mkUUID
                Graph.addNode loc u1 "foo a b c" (atXPos 30.0)
        it "removes arguments in toplevel defs and adds a node" $ let
            initialCode = [r|
                def main a bar c:
                    foo bar
                    baz
                |]
            expectedCode = [r|
                def main:
                    foo bar
                    baz1 = baz
                    foo1 = foo a b c
                    baz1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                (input, _) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc (outPortRef input [Port.Projection 1])
                Graph.removePort loc (outPortRef input [Port.Projection 1])
                Graph.removePort loc (outPortRef input [Port.Projection 0])
                u1 <- mkUUID
                Graph.addNode loc u1 "foo a b c" (atXPos 30.0)

        it "renames function port" $ let
            initialCode = [r|
                def main baz a:
                    foo a
                    a
                |]
            expectedCode = [r|
                def main baz newName:
                    foo newName
                    newName
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                (input, _) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.renamePort loc (outPortRef input [Port.Projection 1]) "newName"
        it "reorders function ports" $ let
            initialCode = [r|
                import Std.Geo
                import Std.Base

                def main baz a:
                    foo a
                    baz
                |]
            expectedCode = [r|
                import Std.Geo
                import Std.Base

                def main a baz:
                    foo a
                    baz
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                (input, _) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc (outPortRef input [Port.Projection 1]) 0
        it "reorders function ports in a lambda" $ let
            initialCode = [r|
                def main:
                    «0»foo = a: b:
                        a + b
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = b: a:
                        a + b
                    c = foo 2 2
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 1]) 0
                return ()
        it "reorders function ports in a lambda 2" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb: ccc :   dddddd:
                        aaaa + bb + ccc + dddddd
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = dddddd  : aaaa: bb :   ccc:
                        aaaa + bb + ccc + dddddd
                    c = foo 2 2
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 3]) 0
        it "reorders function ports in a lambda 3" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb: ccc :   dddddd:
                        aaaa + bb + ccc + dddddd
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = bb  : ccc: dddddd :   aaaa:
                        aaaa + bb + ccc + dddddd
                    c = foo 2 2
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 0]) 3
        it "reorders function ports in a lambda 4" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb: ccc :   dddddd:
                        aaaa + bb + ccc + dddddd
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = bb  : aaaa: ccc :   dddddd:
                        aaaa + bb + ccc + dddddd
                    c = foo 2 2
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 0]) 1
        it "reorders function ports in a lambda 5" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb: ccc :   dddddd:
                        aaaa + bb + ccc + dddddd
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = bb  : ccc: aaaa :   dddddd:
                        aaaa + bb + ccc + dddddd
                    c = foo 2 2
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 0]) 2
        it "reorders function ports in an inner def" $ let
            initialCode = [r|
                def main:
                    «4»def func1 a b:
                        «2»number1 = 30
                        «1»x=1
                        «3»a = x
                        a
                    None

                def kek a b: None
                ### META {"metas":[{"marker":4,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-80,"_vector2_x":512}}}},{"marker":2,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":80,"_vector2_x":80}}}},{"marker":1,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":176,"_vector2_x":176}}}},{"marker":3,"meta":{"_displayResult":false,"_selectedVisualizer":["base: json","base/json/json.html"],"_position":{"fromPosition":{"_vector2_y":-80,"_vector2_x":336}}}}]}
                |]
            expectedCode = [r|
                def main:
                    def func1 b a:
                        number1 = 30
                        x=1
                        a = x
                        a
                    None

                def kek a b: None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just func1 <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 4
                let loc' = loc |>| func1
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 0]) 1
        it "removes last port in top-level def" $ \env -> do
            let initialCode = normalizeLunaCode [r|
                    def foo aaaa:
                        «0»c = aaaa + 2
                        c
                    |]
            code <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc initialCode
                [foo] <- Graph.getNodes loc
                let loc' = loc |>= foo ^. Node.nodeId
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 0])
                code <- Graph.withUnit loc $ use Graph.code
                return code
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
                «1»def foo:
                    «0»c = aaaa + 2
                    c
                |]
        it "removes last port in nested def" $ \env -> do
            let initialCode = normalizeLunaCode [r|
                    def main:
                        «2»def foo aaaa:
                            «0»c = aaaa + 2
                            c
                        «1»d = foo 3
                        d
                    |]
            (inputSidebar, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc initialCode
                [main] <- Graph.getNodes loc
                let loc' = loc |>= main ^. Node.nodeId
                nodes <- Graph.getNodes loc'
                let Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                    loc'' = loc' |>| foo ^. Node.nodeId
                (input, _) <- Graph.withGraph loc'' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc'' (outPortRef input [Port.Projection 0])
                code <- Graph.withUnit loc $ use Graph.code
                inputSidebar <- Graph.withGraph loc'' $ runASTOp $ GraphBuilder.buildInputSidebar input
                return (inputSidebar, code)
            inputSidebar ^. Node.isDef `shouldBe` True
            normalizeLunaCode code `shouldBe` normalizeLunaCode [r|
                «3»def main:
                    «2»def foo:
                        «0»c = aaaa + 2
                        c
                    «1»d = foo 3
                    d
                |]
        it "doesn't remove last port in a lambda" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa:
                        aaaa + 3
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = aaaa:
                        aaaa + 3
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 0])
                    `catch` (\(_e::ASTModify.CannotRemovePortException) -> return ())
                Graph.setNodeExpression loc c "foo 3 3"
        it "removes port in a lambda" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb:
                        aaaa + bb
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = aaaa:
                        aaaa + bb
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 1])
                Graph.setNodeExpression loc c "foo 3 3"
        it "removes port in a lambda 2" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb:
                        aaaa + bb
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = bb:
                        aaaa + bb
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 0])
                Graph.setNodeExpression loc c "foo 3 3"
        it "removes and adds port" $ let
            initialCode = [r|
                def main:
                    «0»lambda = x: y: x
                    «1»c = lambda 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    lambda = x: a: x
                    c = lambda 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just lambda <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| lambda
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 1])
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                Graph.setNodeExpression loc c "lambda 3 3"
        it "removes and adds port 2" $ let
            initialCode = [r|
                def main:
                    «0»lambda = x: y: x
                    «1»c = lambda 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    lambda = a: y: x
                    c = lambda 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just lambda <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| lambda
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (outPortRef input [Port.Projection 0])
                Graph.addPort loc' (outPortRef input [Port.Projection 0])
                Graph.setNodeExpression loc c "lambda 3 3"
        it "adds port in a lambda" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb:
                        aaaa + bb
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = aaaa  : a: bb:
                        aaaa + bb
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                Graph.setNodeExpression loc c "foo 3 3"
        it "adds port in a lambda 2" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb:
                        aaaa + bb
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = a: aaaa  : bb:
                        aaaa + bb
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 0])
                Graph.setNodeExpression loc c "foo 3 3"
        it "adds port in a lambda 3" $ let
            initialCode = [r|
                def main:
                    «0»foo = aaaa  : bb   :
                        aaaa + bb
                    «1»c = foo 2 2
                    c
                |]
            expectedCode = [r|
                def main:
                    foo = aaaa  : bb: a   :
                        aaaa + bb
                    c = foo 3 3
                    c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 2])
                Graph.setNodeExpression loc c "foo 3 3"
        it "connect to left section" $ let
            initialCode = [r|
                def main:
                    «0»p1 = *5
                    «1»n1 = 5
                    n1
                |]
            expectedCode = [r|
                def main:
                    n1 = 5
                    p1 = n1*5
                    n1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just p1, Just n1] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0,1]
                Graph.connect loc (outPortRef n1 []) (InPortRef' $ inPortRef p1 [Port.Arg 0])
        it "connects to self of left section" $ let
            initialCode = [r|
                def main:
                    «1»n1 = 5
                    «0»p1 = *5
                    n1
                |]
            expectedCode = [r|
                def main:
                    n1 = 5
                    p1 = n1.*5
                    n1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just p1, Just n1] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0,1]
                Graph.connect loc (outPortRef n1 []) (InPortRef' $ inPortRef p1 [Port.Self])
        it "connect to left section and disconnect" $ let
            initialCode = [r|
                def main:
                    «0»p1 = *5
                    «1»n1 = 5
                    n1
                |]
            expectedCode = [r|
                def main:
                    n1 = 5
                    p1 = _*5
                    n1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just p1, Just n1] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0,1]
                Graph.connect loc (outPortRef n1 []) (InPortRef' $ inPortRef p1 [Port.Arg 0])
                Graph.disconnect loc (inPortRef p1 [Port.Arg 0])
        it "connect, disconnect and connect to second argument of left section" $ let
            initialCode = [r|
                def main:
                    «0»p1 = *5
                    «1»n1 = 5
                    n1
                |]
            expectedCode = [r|
                def main:
                    n1 = 5
                    p1 = _*n1
                    n1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just p1, Just n1] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0,1]
                Graph.connect loc (outPortRef n1 []) (InPortRef' $ inPortRef p1 [Port.Arg 0])
                Graph.disconnect loc (inPortRef p1 [Port.Arg 0])
                Graph.connect loc (outPortRef n1 []) (InPortRef' $ inPortRef p1 [Port.Arg 1])
        it "adds port that would be shadowed" $ let
            initialCode = [r|
                def main:
                    None
                def foo:
                    «0»a = "text"
                    «1»b = 4
                    «2»s = b + _
                    a
                |]
            expectedCode = [r|
                def main:
                    None
                def foo c:
                    a = "text"
                    b = 4
                    s = b + c
                    a
                |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                let loc = GraphLocation file def
                funs <- Graph.getNodes loc
                let Just foo = find (\n -> n ^. Node.name == Just "foo") funs
                    fooLoc = (loc |>= foo ^. Node.nodeId)
                Just s <- (find (\n -> n ^. Node.name == Just "s")) <$> Graph.getNodes fooLoc
                (i, _) <- Graph.withGraph fooLoc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPortWithConnections fooLoc (outPortRef i [Port.Projection 0]) Nothing [InPortRef' $ inPortRef (s ^. Node.nodeId) [Port.Arg 1]]
                return ()
        it "reorders nodes if required for connection" $ let
            initialCode = [r|
                def main:
                    «0»foo = 1
                    «1»bar = _ + 2
                    «2»baz = foo + 1
                    «3»quux = baz + 3
                    quux
                |]
            expectedCode = [r|
                def main:
                    foo = 1
                    baz = foo + 1
                    quux = baz + 3
                    bar = quux + 2
                    quux
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just bar, Just quux] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [1,3]
                Graph.connect loc (outPortRef quux []) (InPortRef' $ inPortRef bar [Port.Arg 0])
        it "reorders nodes if required for connection - cyclic case" $ let
            initialCode = [r|
                def main:
                    «0»sum1 = _ + 2
                    «1»sum2 = sum1 + 4
                    None
                |]
            expectedCode = [r|
                def main:
                    sum1 = sum2 + 2
                    sum2 = sum1 + 4
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Just sum1, Just sum2] <- Graph.withGraph loc $ runASTOp $ mapM Graph.getNodeIdForMarker [0,1]
                Graph.connect loc (outPortRef sum2 []) (InPortRef' $ inPortRef sum1 [Port.Arg 0])
        it "sets expression for lambdas with markers inside" $ let
            initialCode = testLuna
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b:
                        lala = 15.0
                        buzz = x: y:
                            x * y
                        pi = 3.14
                        n = buzz a lala
                        m = buzz b pi
                        m + n
                    c = 4.0
                    bar = foo 8.0 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Graph.setNodeExpression loc foo "a: b:\n        lala = 15.0\n        buzz = x: y:\n            x * y\n        pi = 3.14\n        n = buzz a lala\n        m = buzz b pi\n        m + n"
        it "pastes string" $ let
            initialCode = [r|
                def main:
                    «4»1
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
                def main:
                    "test"
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.pasteText loc [Range 14 15] ["\"test\""]
        it "pastes string 2" $ let
            initialCode = [r|
                def main:
                    f = ""
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
                def main:
                    f = "test"
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.pasteText loc [Range 18 20] ["\"test\""]
        it "pastes a function to text" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def foo:
                    url = "http://example.com"
                    request = Http.get url
                    response = request . perform
                    None
                def main:
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Graph.pasteText loc [Range 0 0] ["def foo:\n    url = \"http://example.com\"\n    request = Http.get url\n    response = request . perform\n    None\n"]
        it "pastes a function to toplevel" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def foo:
                    url = "http://example.com"
                    request = Http.get url
                    response = request . perform
                    None
                def main:
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \(GraphLocation file _) -> do
                Graph.paste (GraphLocation file def) (Position.fromTuple (-300, 0)) "def foo:\n    url = \"http://example.com\"\n    request = Http.get url\n    response = request . perform\n    None"
        it "pastes a function to functionlevel" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    def foo:
                        url = "http://example.com"
                        request = Http.get url
                        response = request . perform
                        None
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                Graph.paste loc (Position.fromTuple (300, 0)) "def foo:\n    url = \"http://example.com\"\n    request = Http.get url\n    response = request . perform\n    None"
                Graph.substituteCode file [(30, 30, "    ")]
                Graph.substituteCode file [(67, 67, "    ")]
                Graph.substituteCode file [(101, 101, "    ")]
                Graph.substituteCode file [(141, 141, "    ")]
        it "copy pastes" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    bar = foo 8 c
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                code <- Graph.copyText loc [Range 50 60]
                Graph.pasteText loc [Range 50 60] [code]
        it "copy pastes a function" $ let
            initialCode = [r|
                def main:
                    None

                «5»def bar:
                    «0»url = "http://example.com"
                    «1»request = Http.get url
                    «2»response = request . perform
                    «3»body1 = response . body
                    «4»toText1 = body1 . toText
                    None

                |]
            expectedCode = [r|
                def main:
                    None

                def bar:
                    url = "http://example.com"
                    request = Http.get url
                    response = request . perform
                    body1 = response . body
                    toText1 = body1 . toText
                    None
                def bar:
                    url = "http://example.com"
                    request = Http.get url
                    response = request . perform
                    body1 = response . body
                    toText1 = body1 . toText
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                code <- Graph.copyText (GraphLocation file def) [Range 19 189]
                Graph.pasteText (GraphLocation file def) [Range 19 19] [code]
                Graph.getGraph loc
        it "sends update with proper code points after paste" $ \env -> do
            let initialCode = "def main:\n\n    print 3.14\n"
                expectedCode = "def main:\n3.14\n    print 3.14\n"
            (start, end, code) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc initialCode
                code <- Graph.copyText loc [Range 21 25]
                Graph.pasteText loc [Range 10 10] [code]
                codeAfter <- Graph.getCode loc
                return (Code.deltaToPoint 0 codeAfter, Code.deltaToPoint (fromIntegral $ Text.length codeAfter) codeAfter, codeAfter)
            liftIO $ do
                start `shouldBe` Point 0 0
                end `shouldBe` Point 14 2
                code `shouldBe` expectedCode
        it "pastes code with meta" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4

                    «3»bar = foo 8 c

                def bar:
                    «4»c = 1
                    «5»t = 2
                |]
            expectedCode = [r|
                def main:
                    pi = 3.14
                    foo = a: b: a + b
                    c = 4
                    c = 4.0
                    bar = foo 8.0 c

                    bar = foo 8 c

                def bar:
                    c = 1
                    t = 2
                |]
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                let paste = [r|c = 4.0
    bar = foo 8.0 c
|]
                Graph.pasteText loc [Range 56 56] [paste]
                Graph.substituteCode file [(71, 71, "    ")]
        it "handles unary minus" $ let
            initialCode = [r|
                def main:
                    «0»k = -1
                |]
            expectedCode = [r|
                def main:
                    k = -3
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just k <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef k []) (Just $ PortDefault.Constant (PortDefault.IntValue (-2)))
                Graph.setPortDefault loc (inPortRef k []) (Just $ PortDefault.Constant (PortDefault.IntValue (-3)))
                negativeIsApp <- Graph.withGraph loc $ runASTOp $ do
                    negativeLit <- ASTRead.getASTTarget k
                    ASTRead.isApp negativeLit
                liftIO $ negativeIsApp `shouldBe` True
        it "unary minus behaves as a literal" $ let
            initialCode = [r|
                def main:
                    «0»k = -1
                |]
            expectedCode = [r|
                def main:
                    k = -1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [k] <- Graph.getNodes loc
                let portsBefore = k ^. Node.inPorts
                    valueBefore = portsBefore ^? LabeledTree.value . Port.state . Port._WithDefault
                liftIO $ valueBefore `shouldBe` (Just $ PortDefault.Constant (PortDefault.IntValue (-1)))
                Graph.setPortDefault loc (inPortRef (k ^. Node.nodeId) [])
                    (Just $ PortDefault.Constant (PortDefault.IntValue (-1)))
                [kAfter] <- Graph.getNodes loc
                let portsAfter = kAfter ^. Node.inPorts
                liftIO $ portsBefore `shouldBe` portsAfter
        it "reads port name" $ let
            initialCode = [r|
                def main:
                    «0»foo = (Just a): (Just (Just b)): a + b
                |]
            expectedCode = [r|
                def main:
                    foo = (Just a): (Just (Just b)): a + b
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                let loc' = loc |>| foo
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                portName0 <- Graph.getPortName loc' (outPortRef input [Port.Projection 0])
                portName1 <- Graph.getPortName loc' (outPortRef input [Port.Projection 1])
                liftIO $ portName0 `shouldBe` "a"
                liftIO $ portName1 `shouldBe` "b"
        it "disconnects alias node" $ let
            initialCode = [r|
                def main:
                    «0»x = 10
                    «1»y = x
                    «2»c = 1000
                |]
            expectedCode = [r|
                def main:
                    x = 10
                    y = None
                    d = 1000
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [Connection _outRef inRef] <- Graph.getConnections loc
                Graph.disconnect loc inRef
                Just c <- find (\n -> n ^. Node.name == Just "c") <$> Graph.getNodes loc
                Graph.renameNode loc (c ^. Node.nodeId) "d"
        it "shows connection for alias after disconnecting self" $ let
            initialCode = [r|
                def main:
                    «0»x = 10
                    «2»c = "foo"
                    «1»y = x
                |]
            expectedCode = [r|
                def main:
                    x = 10
                    c = "foo"
                    y = x
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just y <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 1
                Just c <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                connsBefore <- Graph.getConnections loc
                Connection _outRef inRef <- Graph.connect loc (outPortRef c []) (InPortRef' $ inPortRef y [Port.Self])
                Graph.disconnect loc inRef
                connsAfter <- Graph.getConnections loc
                liftIO $ connsAfter `shouldBe` connsBefore
        it "connects pattern-matched variable from list to output" $ let
            initialCode = [r|
                def main:
                    «0»list1 = [1,2,3]
                    «1»[a,b,c] = list1
                    None
                |]
            expectedCode = [r|
                def main:
                    list1 = [1,2,3]
                    [a,b,c] = list1
                    a
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.getNodes loc
                let Just match = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "[a, b, c]") nodes
                (_, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.connect loc (outPortRef match [Port.Projection 0]) (InPortRef' $ inPortRef output [])
        it "connects pattern-matched variable from tuple to output" $ let
            initialCode = [r|
                def main:
                    «0»list1 = (1,2,3)
                    «1»(a,b,c) = list1
                    None
                |]
            expectedCode = [r|
                def main:
                    list1 = (1,2,3)
                    (a,b,c) = list1
                    a
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                nodes <- Graph.getNodes loc
                let Just match = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "(a, b, c)") nodes
                (_, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.connect loc (outPortRef match [Port.Projection 0]) (InPortRef' $ inPortRef output [])
        it "names anonymous node" $ let
            initialCode = [r|
                def main:
                    «0»(1,2,3)
                    None
                |]
            expectedCode = [r|
                def main:
                    tuple1 = (1,2,3)
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just t <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.renameNode loc t "tuple1"
        it "connects to node f . g" $ let
            initialCode = [r|
                def main:
                    «0»list1 = [1,2,3]
                    None
                |]
            expectedCode = [r|
                def main:
                    list1 = [1,2,3]
                    map1 = list1 . map +1 . map +2
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "map +1 . map +2" (atXPos 300)
                Just l <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.connect loc (outPortRef l []) (InPortRef' $ inPortRef u1 [Port.Self, Port.Self])
        it "undos add port with connections" $ let
            initialCode = [r|
                def main:
                    «2»lambda1 = x:
                        «3»id1 = id
                    None

                ### META {"metas":[{"marker":2,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-112,"_vector2_x":-32}}}},{"marker":3,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-112,"_vector2_x":-112}}}}]}
                |]
            expectedCode = [r|
                def main:
                    lambda1 = x:
                        id1 = id a
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just lambda1 <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                let loc' = loc |>| lambda1
                nodes <- Graph.getNodes loc'
                let Just id1 = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "id1") nodes
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPortWithConnections loc' (outPortRef input [Port.Projection 1]) Nothing [InPortRef' $ inPortRef id1 [Port.Arg 0]]
                Graph.removePort loc' (outPortRef input [Port.Projection 1])
        it "add port updates lambda length - core bug" $ let
            initialCode = [r|
                def main:
                    «2»lambda1 = x:
                        «3»id1 = id
                    None

                ### META {"metas":[{"marker":2,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-112,"_vector2_x":-32}}}},{"marker":3,"meta":{"_displayResult":false,"_selectedVisualizer":null,"_position":{"fromPosition":{"_vector2_y":-112,"_vector2_x":-112}}}}]}
                |]
            expectedCode = [r|
                def main:
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just lambda1 <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 2
                let loc' = loc |>| lambda1
                nodes <- Graph.getNodes loc'
                let Just id1 = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "id1") nodes
                (input, _) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.addPortWithConnections loc' (outPortRef input [Port.Projection 1]) Nothing [InPortRef' $ inPortRef id1 [Port.Arg 0]]
                Graph.removeNodes loc [lambda1]
                -- uncommenting line below causes a crash, suggesting that something is broken
                -- Graph.withGraph loc $ runASTOp $ AST.dumpGraphViz "a"
        it "maintains connection to output after adding a node" $ let
            initialCode = [r|
                def main:
                    «0»(1,2,3)
                |]
            expectedCode = [r|
                def main:
                    tuple1 = (1,2,3)
                    first1 = first
                    tuple1
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "first" (atXPos 300)
        it "interprets Fibonacci program" $ \env -> do
            Temp.withSystemTempDirectory "luna-fileloadspec" $ \path -> do
                (res, _st) <- runEmp env $ do
                    let initialCode = [r|
                            import Std.Base
                            def fib n:
                                if n < 2 then 1 else fib (n-1) + fib (n-2)

                            def main:
                                a = fib 10
                                a
                            |]
                    Right pkgPath <- Package.genPackageStructure (path </> "Fibonacci") Nothing def
                    let mainLuna = pkgPath </> "src" </> "Main.luna"
                    Library.createLibrary Nothing mainLuna
                    let loc = GraphLocation mainLuna $ Breadcrumb []
                    Graph.loadCode loc $ normalizeLunaCode initialCode
                    [main] <- filter (\n -> n ^. Node.name == Just "main") <$> Graph.getNodes loc
                    let loc' = GraphLocation mainLuna $ Breadcrumb [Definition (main ^. Node.nodeId)]
                    Graph.withUnit loc $ do
                        g <- use Graph.userState
                        let root = g ^. Graph.clsClass
                        rooted <- runASTOp $ Store.serializeWithRedirectMap root
                        return (loc', g, rooted)
                withResult res $ \(loc, g, rooted) -> do
                    pmState <- Graph.defaultPMState
                    let cs = Graph.CommandState pmState $ InterpreterEnv (return ()) g [] def def def def
                    runEmpire env cs $ Typecheck.run loc g rooted True False
                let updates = env ^. to _updatesChan
                ups <- atomically $ unfoldM (tryReadTChan updates)
                let _ResultUpdate = prism ResultUpdate $ \n -> case n of
                        ResultUpdate a -> Right a
                        _              -> Left n
                let [fibUpdate] = ups ^.. traverse . _ResultUpdate . NodeResult.value
                fibUpdate `shouldBe` NodeValue "89" (Just (Value "89"))
        it "does not display connection to itself on anonymous nodes" $ let
            initialCode = [r|
                def main:
                    4
            |]
            in specifyCodeChange initialCode initialCode $ \loc -> do
                [_node] <- Graph.getNodes loc
                selfConn <- filter (\c -> c ^. Connection.src . PortRef.srcNodeId == c ^. Connection.dst . PortRef.dstNodeId) <$> Graph.getConnections loc
                liftIO $ selfConn `shouldBe` mempty
        it "moves lines in a file" $ let
            initialCode = [r|
                def main:
                    «0»pi = 3.14
                    «1»foo = a: b: «4»a + b
                    «2»c = 4
                    «3»bar = foo 8 c
                |]
            expectedCode = [r|
def main:
    pi = 3.14
    c = 4
    foo = a: b: a + b
    bar = foo 8 c
                |]
            in specifyCodeChange initialCode expectedCode $ \_ -> do
                Graph.substituteCodeFromPoints "/TestProject" [ TextDiff (Just (Point 0 2, Point 0 3)) "" Nothing
                                                              , TextDiff (Just (Point 0 4, Point 0 4)) "    foo = a: b: a + b\n" Nothing
                                                              ]
        it "rename from tuple to var" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    tuple1 = (1, 2)
                    x = tuple1
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "(1, 2)" def
                Graph.addNode loc u2 "tuple1" def
                Graph.renameNode loc u2 "(x, y)"
                Graph.renameNode loc u2 "x"
        it "too long grouped as pattern" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    number1 = 1
                    x = number1
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "1" def
                Graph.addNode loc u2 "number1" def
                Graph.renameNode loc u2 "(  x    )"
                Graph.renameNode loc u2 "x"
        it "changes inner output and renames node outside" $ let
            initialCode = [r|
                def main:
                    foo = x: y:
                        aaa = 1
                        b = 2
                        c = 3
                        aaa
                    bar = 444
                    bar

                def quux:
                    b = 3
                    c = 4
                    b
                |]
            expectedCode = [r|
                def main:
                    foo = x: y:
                        aaa = 1
                        b = 2
                        c = 3
                        c
                    pppppp = 444
                    pppppp

                def quux:
                    baz = 3
                    c = 400
                    baz
                |]
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                nodes <- Graph.getNodes loc
                let Just foo = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "foo") nodes
                let Just bar = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "bar") nodes
                let loc' = loc |>| foo
                (_, output) <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.getEdgePortMapping
                nodesFoo <- Graph.getNodes loc'
                let Just c = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "c") nodesFoo
                Graph.connect loc' (outPortRef c []) (InPortRef' $ inPortRef output [])
                Graph.renameNode loc bar "pppppp"
                let top' = GraphLocation file def
                nodesTop <- Graph.getNodes top'
                let Just quux = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "quux") nodesTop
                nodesQuux <- Graph.getNodes (top' |>= quux)
                let Just cQuux = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "c") nodesQuux
                let Just b = (view Node.nodeId) <$> find (\n -> n ^. Node.name == Just "b") nodesQuux
                Graph.renameNode (top' |>= quux) b "baz"
                Graph.setNodeExpression (top' |>= quux) cQuux "400"
        it "does not autoconnect to tuple literal" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    number1 = 1
                    tuple1 = (1,2.0)
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "1" def
                Graph.addNodeWithConnection loc (convert u2) "(1,2.0)" def (Just u1)
                tuple <- Graph.withGraph loc $ runASTOp $ GraphBuilder.buildNode u2
                liftIO $ tuple ^. Node.code `shouldBe` "(1,2.0)"
        it "does not autoconnect to pattern match" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    a=(1,2)
                    (x,y)=a
                    succ1 = succ
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                u3 <- mkUUID
                Graph.addNode loc u1 "a=(1,2)" def
                Graph.addNode loc u2 "(x,y)=a" def
                Graph.addNodeWithConnection loc (convert u3) "succ" def (Just u2)
        it "shows connection after autoconnecting function to output" $ let
            initialCode = [r|
                def main:
                    def foo x y: x + 1
                |]
            expectedCode = [r|
                def main:
                    def foo x y: x + 1
                    number1 = 5
                    foo
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [foo] <- Graph.getNodes loc
                u1 <- mkUUID
                Graph.addNode loc u1 "5" $ atXPos 50.0
                [conn] <- Graph.getConnections loc
                (_, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                liftIO $ conn `shouldBe` Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef output [])
        it "shows connection to left section" $ let
            initialCode = [r|
                def main:
                    foo = 7
                    None
                |]
            expectedCode = [r|
                def main:
                    foo = 7
                    expr1 = + foo
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [foo] <- Graph.getNodes loc
                u1 <- mkUUID
                Graph.addNode loc u1 "+ foo" $ atXPos 50.0
                [conn] <- Graph.getConnections loc
                liftIO $ conn `shouldBe` Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef u1 [Port.Arg 1])
        it "shows connection to right section" $ let
            initialCode = [r|
                def main:
                    foo = 7
                    None
                |]
            expectedCode = [r|
                def main:
                    foo = 7
                    expr1 = foo +
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                [foo] <- Graph.getNodes loc
                u1 <- mkUUID
                Graph.addNode loc u1 "foo +" $ atXPos 50.0
                [conn] <- Graph.getConnections loc
                liftIO $ conn `shouldBe` Connection (outPortRef (foo ^. Node.nodeId) []) (inPortRef u1 [Port.Arg 0])
        it "connects nested patternmatch to output" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    a=[((Just 1), "foo")]
                    [((Just i), j)]=a
                    i
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "a=[((Just 1), \"foo\")]" def
                Graph.addNode loc u2 "[((Just i), j)]=a" def
                (_, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.connect loc (outPortRef u2 [Port.Projection 0, Port.Projection 0, Port.Projection 0]) (InPortRef' $ inPortRef output [])
        it "connects nested patternmatch to output 2" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    a= Just [  (1  , Foo 9)]
                    Just [(i,  Foo  b) ] =a
                    b
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "a= Just [  (1  , Foo 9)]" def
                Graph.addNode loc u2 "Just [(i,  Foo  b) ] =a" def
                (_, output) <- Graph.withGraph loc $ runASTOp $ GraphBuilder.getEdgePortMapping
                Graph.connect loc (outPortRef u2 [Port.Projection 0, Port.Projection 0, Port.Projection 1, Port.Projection 0]) (InPortRef' $ inPortRef output [])
        it "sets tuple port defaults" $ let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    tuple1 = (False, False)
                    tuple2 = ( 1, 2,   100)
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                u1 <- mkUUID
                u2 <- mkUUID
                Graph.addNode loc u1 "(True, False)" def
                Graph.addNode loc u2 "( 1, 2,   3)" def
                Graph.setPortDefault loc (inPortRef u1 [Port.Arg 0]) (Just (PortDefault.Constant (PortDefault.BoolValue False)))
                Graph.setPortDefault loc (inPortRef u2 [Port.Arg 2]) (Just (PortDefault.Constant (PortDefault.IntValue 100)))
        it "sets string port defaults" $ let
            initialCode = [r|
                def main:
                    def foo x y: x + 1
                    «0»foo 19 "heyyyyyy"
                    None
                |]
            expectedCode = [r|
                def main:
                    def foo x y: x + 1
                    foo 19 "heyy"
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc -> do
                Just fooApp <- Graph.withGraph loc $ runASTOp $
                    Graph.getNodeIdForMarker 0
                Graph.setPortDefault loc (inPortRef fooApp [Port.Arg 1])
                    (Just (PortDefault.Constant (PortDefault.TextValue "heyyyy")))
                Graph.setPortDefault loc (inPortRef fooApp [Port.Arg 1])
                    (Just (PortDefault.Constant (PortDefault.TextValue "heyy")))
        it "throws exception on setting out of bounds tuple element" $ \env -> let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    tuple1 = ( 1, 2,   3)
                    None
                |]
            tupleOOB :: Selector ASTBuilder.TupleElementOutOfBoundsException
            tupleOOB = const True
            in specifyCodeChange initialCode expectedCode (\loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "( 1, 2,   3)" def
                Graph.setPortDefault loc (inPortRef u1 [Port.Arg 5]) (Just (PortDefault.Constant (PortDefault.IntValue 0)))
                ) env `shouldThrow` tupleOOB
        it "throws exception on setting negative out of bounds tuple element" $ \env -> let
            initialCode = [r|
                def main:
                    None
                |]
            expectedCode = [r|
                def main:
                    tuple1 = ( 1, 2,   3)
                    None
                |]
            tupleOOB :: Selector ASTBuilder.TupleElementOutOfBoundsException
            tupleOOB = const True
            in specifyCodeChange initialCode expectedCode (\loc -> do
                u1 <- mkUUID
                Graph.addNode loc u1 "( 1, 2,   3)" def
                Graph.setPortDefault loc (inPortRef u1 [Port.Arg (-1)]) (Just (PortDefault.Constant (PortDefault.IntValue 0)))
                ) env `shouldThrow` tupleOOB
        it "redos collapsing to a function" $ let
            initialCode = [r|
                «5»def main:
                    None

                «6»def bar:
                    «0»url = "http://example.com"
                    «1»request = Http.get url
                    «2»response = request . perform
                    «3»body1 = response . body
                    «4»toText1 = body1 . toText
                    None
                |]
            expectedCode = [r|
                def main:
                    None

                def func1 url:
                    request = Http.get url
                    response = request . perform
                    response

                def bar:
                    url = "http://example.com"
                    response = func1 url
                    body1 = response . body
                    toText1 = body1 . toText
                    None
                |]
            in specifyCodeChange initialCode expectedCode $ \loc@(GraphLocation file _) -> do
                let top' = GraphLocation file def
                funs <- Graph.getNodes top'
                let Just bar = find (\n -> n ^. Node.name == Just "bar") funs
                    bar' = GraphLocation file (Breadcrumb [Definition (bar ^. Node.nodeId)])
                ids <- Graph.withGraph bar' $ runASTOp $ mapM (Graph.getNodeIdForMarker) [1..2]
                nodes <- Graph.getNodes bar'
                (undoCode, undoCache) <- (,) <$> Graph.withUnit top' (use Graph.code) <*> Graph.prepareNodeCache top'
                Graph.collapseToFunction bar' $ map fromJust ids
                code <- Graph.getCode top'
                liftIO $ code `shouldBe` normalizeLunaCode expectedCode
                Graph.withUnit top' $ Graph.nodeCache .= undoCache
                Graph.loadCode loc undoCode
                code' <- Graph.withUnit top' $ use Graph.code
                liftIO $ code' `shouldBe` normalizeLunaCode initialCode
                nodes' <- Graph.getNodes bar'
                liftIO $ nodes `shouldBe` nodes'
                Graph.collapseToFunction bar' $ map fromJust ids
