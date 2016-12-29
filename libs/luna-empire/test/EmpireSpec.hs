{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module EmpireSpec (spec) where

import           Data.Foldable                 (toList)
import           Data.List                     (find, stripPrefix)
import qualified Data.Map                      as Map
import qualified Empire.API.Data.Graph         as Graph
import           Empire.API.Data.GraphLocation (GraphLocation(..))
import qualified Empire.API.Data.Node          as Node (NodeType(ExpressionNode), canEnter,
                                                        expression, name, nodeId, nodeType, ports)
import qualified Empire.API.Data.Port          as Port
import           Empire.API.Data.PortRef       (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep       (TypeRep(TCons))
import           Empire.API.Data.ValueType     (ValueType(TypeIdent))
import           Empire.ASTOp                  (runASTOp)
import qualified Empire.ASTOps.Deconstruct     as ASTDeconstruct
import qualified Empire.ASTOps.Parse           as Parser
import qualified Empire.ASTOps.Read            as ASTRead
import qualified Empire.Commands.AST           as AST (isTrivialLambda, rhsIsLambda)
import qualified Empire.Commands.Graph         as Graph (addNode, connect, getGraph, getNodes,
                                                         getConnections, removeNodes, withGraph,
                                                         renameNode)
import qualified Empire.Commands.GraphBuilder  as GraphBuilder
import           Empire.Commands.Library       (withLibrary)
import qualified Empire.Commands.Typecheck     as Typecheck (run)
import           Empire.Data.Graph             (NodeIDTarget(..), ast, nodeMapping)
import qualified Empire.Data.Library           as Library (body)
import           Empire.Empire                 (InterpreterEnv(..))
import           Prologue                      hiding (mapping, toList, (|>))

import           Test.Hspec (Spec, around, describe, expectationFailure, it,
                             shouldBe, shouldContain, shouldSatisfy, shouldMatchList)

import           EmpireUtils


spec :: Spec
spec = around withChannels $ do
    describe "luna-empire" $ do
        it "descends into `def foo` and asserts two edges inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                topLevel <- graphIDs top
                n1Level <- Graph.getNodes (top |> u1)
                return (topLevel, n1Level)
            withResult res $ \(topLevel, n1Level) -> do
                [u1] `shouldMatchList` topLevel
                n1Level `shouldSatisfy` ((== 2) . length)
                excludeEdges n1Level `shouldSatisfy` null
        it "asserts things about `def foo`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
            withResult res $ \node -> do
                node ^. Node.name `shouldBe` "foo"
                node ^. Node.nodeType `shouldBe` Node.ExpressionNode "-> $in0 in0"
                node ^. Node.canEnter `shouldBe` True
        it "makes connection to output edge" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef out (Port.Arg 0))
                uncurry (Graph.connect loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 1) . length)
                head connections `shouldBe` conn
        it "connects input edge to succ (Self)" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input Port.All, InPortRef u2 Port.Self)
                uncurry (Graph.connect loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "connects input edge to dummy node (Arg 0)" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input Port.All, InPortRef u2 (Port.Arg 0))
                uncurry (Graph.connect loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "has proper connection inside `def foo`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                conns <- Graph.getConnections loc'
                Just edges <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                return (conns, edges)
            withResult res $ \(connections, (inputEdge, outputEdge)) -> do
                connections `shouldMatchList` [(OutPortRef inputEdge (Port.Projection 0), InPortRef outputEdge (Port.Arg 0))]
        it "shows connection inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode loc' u2 "4" def
                Graph.addNode loc' u3 "succ" def
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef u3 Port.Self)
                uncurry (Graph.connect loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(ref, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [ref]
        it "creates two nested lambdas and a node inside" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                Graph.addNode (top |> u1) u2 "def bar" def
                let loc = top |> u1 |> u2
                Graph.addNode loc u3 "4" def
                graphIDs loc
            withResult res $ \ids -> do
                u3 `shouldSatisfy` (`elem` ids)
                u1 `shouldSatisfy` (`notElem` ids)
                u2 `shouldSatisfy` (`notElem` ids)
        it "cannot enter lambda applied to value" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                Graph.addNode top u2 "4" def
                Graph.connect top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                Graph.getGraph top
            withResult res $ \g -> do
                let Just lambdaNode = find ((== u1) . view Node.nodeId) $ Graph._nodes g
                lambdaNode ^. Node.canEnter `shouldBe` False
        it "has no null node inside `def foo`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                Graph.getNodes (top |> u1)
            withResult res $ \(excludeEdges -> ids) -> do
                ids `shouldSatisfy` null
        it "int literal has no nodes inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                graphIDs $ top |> u1
            case res of
                Left err -> case stripPrefix "CannotEnterNodeException" err of
                    Just _ -> return ()
                    _      -> expectationFailure err
                Right _  -> expectationFailure "should throw"
        it "properly typechecks input nodes" $ \env -> do
            u1 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = top
                withLibrary pid lid (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'', _) <- runEmp' env st g' $ do
                    Graph.getNodes $ top |> u1
                withResult res'' $ \nodes' -> do
                    let Just input = find ((== "inputEdge") . view Node.name) nodes'
                        ports' = toList $ input ^. Node.ports
                        types = map (view Port.valueType) ports'
                    types `shouldMatchList` [TypeIdent (TCons "Int" []), TypeIdent (TCons "Int" [])]
        it "properly typechecks output nodes" $ \env -> do
            u1 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = top
                withLibrary pid lid (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'',_) <- runEmp' env st g' $ do
                    Graph.getNodes $ top |> u1
                withResult res'' $ \nodes' -> do
                    let Just output' = find ((== "outputEdge") . view Node.name) nodes'
                        ports' = toList $ output' ^. Node.ports
                        types = map (view Port.valueType) ports'
                    types `shouldBe` [TypeIdent (TCons "Int" [])]
        it "adds lambda nodeid to node mapping" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                Graph.withGraph top $ use nodeMapping
            withResult res $ \(toList -> mapping) -> do
                let isLambdaNode n = case n of
                        AnonymousNode _ -> True
                        _               -> False
                    lambdaNodes = filter isLambdaNode mapping
                lambdaNodes `shouldSatisfy` (not . null)
        it "puts + inside plus lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "-> $a $b a + b" def
                Graph.getNodes loc'
            withResult res $ \(excludeEdges -> nodes) -> do
                nodes `shouldSatisfy` ((== 1) . length)
                head nodes `shouldSatisfy` (\a -> a ^. Node.nodeType . Node.expression == "a + b")
        it "places connections between + node and output" $ \env -> do
          u1 <- mkUUID
          res <- evalEmp env $ do
              let loc' = top |> u1
              Graph.addNode top u1 "-> $a $b a + b" def
              Graph.getConnections loc'
          withResult res $ \conns -> do
              conns `shouldSatisfy` ((== 1) . length)
        it "cleans after removing `def foo` with `4` inside connected to output" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef out (Port.Arg 0))
                uncurry (Graph.connect loc') referenceConnection
                Graph.removeNodes top [u1]
                Graph.withGraph top $ (,) <$> use ast <*> use nodeMapping
            withResult res $ \(endAst, mapping) -> do
                mapping `shouldSatisfy` Map.null
                endAst `shouldSatisfy` astNull
        it "RHS of `def foo` is Lam" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "def foo" def
                Graph.withGraph top $ runASTOp $ AST.rhsIsLambda u1
            withResult res $ \a -> a `shouldBe` True
        it "`def foo` is trivial - has output connected to input" $ \env -> do
            res <- evalEmp env $ do
                Graph.withGraph top $ do
                   (_, ref) <- runASTOp $ Parser.parseExpr "def foo"
                   runASTOp $ AST.isTrivialLambda ref
            withResult res $ \a -> a `shouldBe` True
        it "changes name of a variable in-place" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo" def
                Graph.renameNode top u1 "bar"
                Graph.getNodes top
            withResult res $ \nodes -> head nodes ^. Node.name `shouldBe` "bar"
    describe "dumpAccessors" $ do
        it "foo.bar" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo.bar" def
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u1
                    ASTDeconstruct.dumpAccessors accs
            withResult res $ \(node, accs) -> do
                node `shouldBe` Nothing
                accs `shouldBe` ["foo", "bar"]
        it "baz ---o foo.bar" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo.bar" def
                Graph.addNode top u2 "baz" def
                Graph.connect top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Self))
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u1
                    (,) <$> ASTRead.getASTVar u2 <*> ASTDeconstruct.dumpAccessors accs
            withResult res $ \(reference, (node, accs)) -> do
                node `shouldBe` Just reference
                accs `shouldBe` ["foo", "bar"]
        it "1 ---> foo ---o bar ---o baz" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.addNode top u2 "foo" def
                Graph.connect top (OutPortRef u1 Port.All) (InPortRef u2 (Port.Arg 0))
                Graph.addNode top u3 "bar" def
                Graph.connect top (OutPortRef u2 Port.All) (InPortRef u3 Port.Self)
                Graph.addNode top u4 "baz" def
                Graph.connect top (OutPortRef u3 Port.All) (InPortRef u4 Port.Self)
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u4
                    (,) <$> ASTRead.getASTVar u3 <*> ASTDeconstruct.dumpAccessors accs
            withResult res $ \(reference, (node, accs)) -> do
                node `shouldBe` Just reference
                accs `shouldBe` ["baz"]
