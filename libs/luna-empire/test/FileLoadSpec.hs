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
import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.GraphBuilder   as GraphBuilder
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
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
bar ‹2›= foo c 6
‹3›print pi
c ‹4›= 3
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
                    , (outPortRef (c ^. Node.nodeId)  [], inPortRef (bar ^. Node.nodeId)  [Port.Arg 0])
                    ]
        it "shows proper changes to expressions" $ \env -> do
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 59 59 "3" (Just 60)
            withResult res $ \(coerce -> Just (rs :: [Parser.ReparsingChange])) -> do
                let unchanged = filter (\x -> case x of Parser.UnchangedExpr{} -> True; _ -> False) rs
                    changed   = filter (\x -> case x of Parser.ChangedExpr{} -> True; _ -> False) rs
                length unchanged `shouldBe` 3
                length changed `shouldBe` 1
        it "does not duplicate nodes on edit" $ \env -> do
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                changes <- Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
                graph   <- Graph.getGraph loc
                return (changes, graph)
            withResult res $ \(coerce -> Just (rs :: [Parser.ReparsingChange]), graph) -> do
                let unchanged = filter (\x -> case x of Parser.UnchangedExpr{} -> True; _ -> False) rs
                    changed   = filter (\x -> case x of Parser.ChangedExpr{} -> True; _ -> False) rs
                length unchanged `shouldBe` 3
                length changed `shouldBe` 1
                let Graph.Graph nodes connections _ _ _ = graph
                    cNodes = filter (\node -> node ^. Node.name == Just "c") nodes
                length cNodes `shouldBe` 1
                let [cNode] = cNodes
                    Just bar = find (\node -> node ^. Node.name == Just "bar") nodes
                connections `shouldMatchList` [
                      (outPortRef (cNode ^. Node.nodeId) [], inPortRef (bar ^. Node.nodeId) [Port.Arg 1])
                    ]
        it "double modification gives proper value" $ \env -> do
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
                Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
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
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 43 43 "3" (Just 44)
                Graph.substituteCode "TestPath" 59 59 "1" (Just 60)
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
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 35 35 "d ‹4›= 10" (Just 36)
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
            let code = [r|pi ‹0›= 3.14

foo ‹1›= a: b: a + b

c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.substituteCode "TestPath" 8 12 ")" (Just 8)
                Graph.substituteCode "TestPath" 8 9 "5" (Just 8)
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
            let code = [r|foo ‹0›= a: b: a + b|]
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
        xit "pi <0>= 3.14" $ \env -> do
            let code = [r|‹0›print 3.14
‹1›delete root
‹2›suspend computer
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..2] $ Graph.markerCodeSpan loc
                -- Graph.markerCodeSpan loc 3
            withResult res $ \span -> do
                return ()
        xit "shows proper expressions ranges" $ \env -> do
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                forM [0..3] $ Graph.markerCodeSpan loc
            withResult res $ \spans -> do
                return ()
        it "adds one node to code" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.getCode top
            withResult res $ \code -> do
                code `shouldBe` "node1 ‹0›= 4\n"
        it "adds one node and updates it" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.setNodeExpression top u1 "5"
                Graph.getCode top
            withResult res $ \code -> do
                code `shouldBe` "node1 ‹0›= 5\n"
        it "assigns nodeids to marked expressions" $ \env -> do
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
|]
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Graph.withGraph loc $ runASTOp $ forM [0..3] Graph.getNodeIdForMarker
            withResult res $ \ids -> do
                ids `shouldSatisfy` (all isJust)
        it "adds one node to existing file" $ \env -> do
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
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
                Graph.getCode loc
            withResult res $ \code -> do
                code `shouldBe` [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
node1 ‹4›= 4
|]
        it "adds one node to existing file and updates it" $ \env -> do
            let code = [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
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
                code `shouldBe` [r|pi ‹0›= 3.14
foo ‹1›= a: b: a + b
c ‹2›= 4
bar ‹3›= foo 8 c
node1 ‹4›= 5
|]
        it "lambda in code can be entered" $ \env -> do
            let code = [r|foo ‹0›= a: a|]
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Library.createLibrary Nothing "TestPath" code
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.withGraph loc $ Graph.loadCode code
                Just foo <- Graph.withGraph loc $ runASTOp $ Graph.getNodeIdForMarker 0
                Graph.addNode loc u1 "node2" def
                Graph.addNode loc u2 "node1" def
                Graph.substituteCode "TestPath" 35 36 "9" Nothing
                Graph.getGraph $ loc |> foo
            withResult res $ \(Graph.Graph nodes connections _ _ _) -> do
                nodes `shouldBe` []
                connections `shouldSatisfy` (not . null)
