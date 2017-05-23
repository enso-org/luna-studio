{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module FileLoadSpec (spec) where

import           Control.Monad                  (forM)
import           Data.Coerce
import           Data.List                      (find)
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           LunaStudio.Data.Breadcrumb     (Breadcrumb (..))
import qualified LunaStudio.Data.Graph          as Graph
import           LunaStudio.Data.GraphLocation  (GraphLocation (..))
import qualified LunaStudio.Data.Node           as Node
import           LunaStudio.Data.NodeMeta       (NodeMeta (..))
import qualified LunaStudio.Data.Port           as Port
import           LunaStudio.Data.PortRef        (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.Position       as Position
import           LunaStudio.Data.TypeRep        (TypeRep (TStar))
import           Empire.ASTOp                   (runASTOp)
import qualified Empire.ASTOps.Parse            as ASTParse
import qualified Empire.ASTOps.Print            as ASTPrint
import           Empire.Data.AST                (SomeASTException)
import qualified Empire.Commands.AST            as AST
import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.GraphBuilder   as GraphBuilder
import qualified Empire.Commands.Lexer          as Lexer
import qualified Empire.Commands.Library        as Library
import qualified Empire.Data.Graph              as Graph (breadcrumbHierarchy)
import qualified Luna.Syntax.Text.Parser.Parser as Parser (ReparsingChange (..), ReparsingStatus (..))

import           Empire.Prelude

import           Test.Hspec                     (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldMatchList,
                                                 shouldSatisfy, shouldStartWith, xit)

import           EmpireUtils

import           Text.RawString.QQ              (r)


spec :: Spec
spec = around withChannels $ do
    describe "file loading" $ do
        it "parses unit" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    bar «2»= foo c 6
    «3»print pi
    c «4»= 3
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                graph <- Graph.withGraph loc $ runASTOp $ GraphBuilder.buildGraph
                return graph
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                let Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                pi ^. Node.code `shouldBe` Just "3.14"
                pi ^. Node.canEnter `shouldBe` False
                let Just foo = find (\node -> node ^. Node.name == Just "foo") nodes
                foo ^. Node.code `shouldBe` Just "a: b: a + b"
                foo ^. Node.canEnter `shouldBe` True
                let Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                bar ^. Node.code `shouldBe` Just "foo c 6"
                bar ^. Node.canEnter `shouldBe` False
                let Just anon = find (\node -> node ^. Node.name == Nothing) nodes
                anon ^. Node.code `shouldBe` Just "print pi"
                anon ^. Node.canEnter `shouldBe` False
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                c ^. Node.code `shouldBe` Just "3"
                c ^. Node.canEnter `shouldBe` False
                connections `shouldMatchList` [
                      (outPortRef (pi ^. Node.nodeId) [], inPortRef (anon ^. Node.nodeId) [Port.Arg 0])
                    ]
        it "does not duplicate nodes on edit" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14

    foo «1»= a: b: a + b

    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "double modification gives proper value" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14

    foo «1»= a: b: a + b

    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 65 65 "3" (Just 66)
                Graph.substituteCode "TestPath" 65 65 "3" (Just 66)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                cNode ^. Node.code `shouldBe` Just "334"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "modifying two expressions give proper values" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14

    foo «1»= a: b: a + b

    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 65 65 "3" (Just 66)
                Graph.substituteCode "TestPath" 85 85 "1" (Just 86)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length nodes `shouldBe` 4
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                cNode ^. Node.code `shouldBe` Just "34"
                bar ^. Node.code `shouldBe` Just "foo 18 c"
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "adding an expression works" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 86 86 "    d «4»= 10\n" (Just 86)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just d = find (\node -> node ^. Node.name == Just "d") nodes
                d ^. Node.code `shouldBe` Just "10"
                let Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "unparseable expression does not sabotage whole file" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 22 26 ")" (Just 26) `catch` (\(_e :: SomeASTException) -> return ())
                Graph.substituteCode "TestPath" 22 23 "5" (Just 23)
                Graph.getGraph loc
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                    Just pi = find (\node -> node ^. Node.name == Just "pi") nodes
                    Just c = find (\node -> node ^. Node.name == Just "c") nodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                pi ^. Node.code `shouldBe` Just "5"
                c ^. Node.code `shouldBe` Just "4"
                bar ^. Node.code `shouldBe` Just "foo 8 c"
                connections `shouldMatchList` [
                      (outPortRef (c ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "enters lambda written in file" $ \env -> do
            let code = [r|def main:
    foo «0»= a: b: a + b|]
                loc = GraphLocation "TestPath" $ Breadcrumb []
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.withGraph (loc |> foo) $ runASTOp $ GraphBuilder.buildGraph
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ = graph
                nodes `shouldSatisfy` ((== 1) . length)
                connections `shouldSatisfy` ((== 3) . length)
    describe "code spans" $ do
        it "pi «0»= 3.14" $ \env -> do
            let code = [r|def main:
    «0»print 3.14
    «1»delete root
    «2»suspend computer
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..2] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (17, 27)
                    , (35, 46)
                    , (54, 70)
                    ]
        it "shows proper expressions ranges" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..3] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                spans `shouldBe` [
                      (14, 26)
                    , (31, 51)
                    , (56, 64)
                    , (69, 85)
                    ]
        it "adds one node to code" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.getCode top
            withResult res $ \code -> do
                code `shouldBe` "def main:\n    node1 «0»= 4\n"
        it "adds one node and updates it" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.markerCodeSpan top 0
                Graph.setNodeExpression top u1 "5"
                Graph.getCode top
            withResult res $ \code -> do
                code `shouldBe` "def main:\n    node1 «0»= 5\n"
        it "assigns nodeids to marked expressions" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.withGraph loc $ runASTOp $ forM [0..3] Graph.getNodeIdForMarker
            withResult res $ \ids -> do
                ids `shouldSatisfy` (all isJust)
        it "adds one node to existing file via node editor" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            u1 <- mkUUID
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                nodeIds <- Graph.withGraph loc $ runASTOp $ forM [0..3] $
                    \i -> (i,) <$> Graph.getNodeIdForMarker i
                spans <- forM [0..3] $ Graph.markerCodeSpan loc
                forM nodeIds $ \(i, Just nodeId) ->
                    Graph.setNodeMeta loc nodeId $ NodeMeta (Position.fromTuple (0, fromIntegral i*10)) False
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (0, 5)) False)
                spans <- forM [0..4] $ Graph.markerCodeSpan loc
                code  <- Graph.getCode loc
                return (spans, code)
            withResult res $ \(spans, code) -> do
                code `shouldBe` [r|def main:
    pi «0»= 3.14
    node1 «4»= 4
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
                spans `shouldBe` [
                      (14, 26)
                    , (48, 68)
                    , (73, 81)
                    , (86, 102)
                    , (31, 43)
                    ]
        it "adds one node to existing file via text" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            u1 <- mkUUID
            (code, spans) <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 65 65 "    d «4»= 6\n" Nothing
                code  <- Graph.getCode loc
                spans <- forM [0..4] $ Graph.markerCodeSpan loc
                return (code, spans)
            code `shouldBe` [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    d «4»= 6
    bar «3»= foo 8 c
|]
            spans `shouldBe` [
                  (14, 26)
                , (31, 51)
                , (56, 64)
                , (82, 98)
                , (69, 77)
                ]
        it "adds one node to existing file and updates it" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            u1 <- mkUUID
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                nodeIds <- Graph.withGraph loc $ runASTOp $ forM [0..3] $
                    \i -> (i,) <$> Graph.getNodeIdForMarker i
                forM nodeIds $ \(i, Just nodeId) ->
                    Graph.setNodeMeta loc nodeId $ NodeMeta (Position.fromTuple (0, fromIntegral i*10)) False
                Graph.addNode loc u1 "4" (NodeMeta (Position.fromTuple (10, 50)) False)
                Graph.setNodeExpression loc u1 "5"
                Graph.getCode loc
            withResult res $ \code -> do
                code `shouldBe` [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
    node1 «4»= 5
|]
        it "lambda in code can be entered" $ \env -> do
            let code = [r|def main:
    foo «0»= a: a|]
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.getGraph $ loc |> foo
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                nodes `shouldBe` []
                connections `shouldSatisfy` (not . null)
        it "lex" $ \env -> do
            let code = [r|def main:
    pi «0»= 3.14
    foo «1»= a: b: a + b
    c «2»= 4
    bar «3»= foo 8 c
|]
            let tokens = Lexer.lexer code
            tokens `shouldSatisfy` (not . null)
            sum (map fst tokens) `shouldBe` Text.length code
