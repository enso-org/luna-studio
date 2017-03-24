{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module EmpireSpec (spec) where

import           Data.Foldable                   (toList)
import           Data.List                       (find, stripPrefix)
import qualified Data.Map                        as Map
import qualified Empire.API.Data.Graph           as Graph
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import qualified Empire.API.Data.Node            as Node (NodeType (ExpressionNode), canEnter, expression, name, nodeId, nodeType, ports)
import           Empire.API.Data.NodeMeta        (NodeMeta (..))
import qualified Empire.API.Data.Port            as Port
import           Empire.API.Data.PortDefault     (PortDefault (Expression))
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep         (TypeRep (TCons, TLam, TStar, TVar))
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Parse             as Parser
import           Empire.ASTOps.Print             (printExpression)
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST (isTrivialLambda, dumpGraphViz)
import qualified Empire.Commands.Graph           as Graph (addNode, addPort, connect, disconnect, getConnections, getGraph,
                                                           getNodeIdSequence, getNodes, movePort, removeNodes, removePort, renameNode,
                                                           renamePort, setNodeExpression, setNodeMeta, withGraph)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import           Empire.Commands.Library         (withLibrary)
import qualified Empire.Commands.Typecheck       as Typecheck (run)
import           Empire.Data.Graph               (breadcrumbHierarchy)
import qualified Empire.Data.Library             as Library (body)
import           Empire.Data.BreadcrumbHierarchy (NodeIDTarget (..))
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (ast, breadcrumbHierarchy)
import qualified Empire.Data.Library             as Library (body)
import           Empire.Empire                   (InterpreterEnv (..))
import           OCI.IR.Class                    (exprs, links)
import           Prologue                        hiding (mapping, toList, (|>))

import           Test.Hspec                      (Spec, around, describe, expectationFailure, it, parallel, shouldBe, shouldContain,
                                                  shouldMatchList, shouldSatisfy, shouldStartWith, xdescribe, xit)

import           EmpireUtils


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "luna-empire" $ do
        it "descends into `foo = a: a` and asserts two edges inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                topLevel <- graphIDs top
                n1Level <- Graph.getNodes (top |> u1)
                return (topLevel, n1Level)
            withResult res $ \(topLevel, n1Level) -> do
                [u1] `shouldMatchList` topLevel
                n1Level `shouldSatisfy` ((== 2) . length)
                excludeEdges n1Level `shouldSatisfy` null
        it "asserts things about `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
            withResult res $ \node -> do
                node ^. Node.name `shouldBe` "foo"
                node ^. Node.nodeType `shouldBe` Node.ExpressionNode "a: a"
                node ^. Node.canEnter `shouldBe` True
        it "makes connection to output edge" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef out (Port.Arg 0))
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 1) . length)
                head connections `shouldBe` conn
        it "connects input with output edge" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.disconnect loc' (InPortRef output (Port.Arg 0))
                let referenceConnection = (OutPortRef input (Port.Projection 0), InPortRef output (Port.Arg 0))
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(ref, connections) -> do
                connections `shouldMatchList` [ref]
        it "connects input edge to succ (Self)" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input (Port.Projection 0), InPortRef u2 Port.Self)
                uncurry (connectToInput loc') referenceConnection
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
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input (Port.Projection 0), InPortRef u2 (Port.Arg 0))
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "has proper connection inside `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "foo = a: a" def
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
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "4" def
                Graph.addNode loc' u3 "succ" def
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef u3 Port.Self)
                uncurry (connectToInput loc') referenceConnection
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
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode (top |> u1) u2 "bar = c: c" def
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
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode top u2 "4" def
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                Graph.getGraph top
            withResult res $ \g -> do
                let Just lambdaNode = find ((== u1) . view Node.nodeId) $ Graph._nodes g
                lambdaNode ^. Node.canEnter `shouldBe` False
        it "has no null node inside `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.getNodes (top |> u1)
            withResult res $ \(excludeEdges -> ids) -> do
                ids `shouldSatisfy` null
        it "int literal has no nodes inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                graphIDs $ top |> u1
            case res of
                Left err -> case stripPrefix "Breadcrumb" err of
                    Just _ -> return ()
                    _      -> expectationFailure err
                Right _  -> expectationFailure "should throw"
        xit "properly typechecks input nodes" $ \env -> do
            u1 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                let GraphLocation file _ = top
                withLibrary file (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'', _) <- runEmp' env st g' $ do
                    Graph.getNodes $ top |> u1
                withResult res'' $ \nodes' -> do
                    let Just input = find ((== "inputEdge") . view Node.name) nodes'
                        ports' = toList $ input ^. Node.ports
                        types = map (view Port.valueType) ports'
                    types `shouldMatchList` [TCons "Int" [], TCons "Int" []]
        xit "properly typechecks output nodes" $ \env -> do
            u1 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                let GraphLocation file _ = top
                withLibrary file (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'',_) <- runEmp' env st g' $ do
                    Graph.getNodes $ top |> u1
                withResult res'' $ \nodes' -> do
                    let Just output' = find ((== "outputEdge") . view Node.name) nodes'
                        ports' = toList $ output' ^. Node.ports
                        types = map (view Port.valueType) ports'
                    types `shouldBe` [TCons "Int" []]
        xit "properly typechecks edges inside mock id" $ \env -> do
            u1 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "__intId" def
                let GraphLocation file _ = top
                withLibrary file (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'',_) <- runEmp' env st g' $ do
                    Graph.getNodes $ top |> u1
                withResult res'' $ \nodes' -> do
                    let Just input' = find ((== "inputEdge") . view Node.name) nodes'
                        inPorts' = toList $ input' ^. Node.ports
                        inputType = map (view Port.valueType) inPorts'
                    let Just output' = find ((== "outputEdge") . view Node.name) nodes'
                        outPorts' = toList $ output' ^. Node.ports
                        outputType = map (view Port.valueType) outPorts'
                    inputType  `shouldBe` [TCons "Int" []]
                    outputType `shouldBe` [TCons "Int" []]
        xit "properly typechecks second id in `mock id -> mock id`" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            (res, st) <- runEmp env $ do
                Graph.addNode top u1 "id" def
                Graph.addNode top u2 "id" def
                connectToInput top (OutPortRef u1 Port.All) (InPortRef u2 (Port.Arg 0))
                let GraphLocation file _ = top
                withLibrary file (use Library.body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run emptyGraphLocation
                (res'',_) <- runEmp' env st g' $ do
                    Graph.withGraph top $ runASTOp $ (,) <$> GraphBuilder.buildNode u1 <*> GraphBuilder.buildNode u2
                withResult res'' $ \(n1, n2) -> do
                    inputPorts n2 `shouldMatchList` [
                          Port.Port (Port.InPortId (Port.Arg 0)) "in" (TLam (TVar "a") (TVar "a")) Port.Connected
                        ]
                    outputPorts n1 `shouldMatchList` [
                          Port.Port (Port.OutPortId Port.All) "Output" (TLam (TVar "a") (TVar "a")) (Port.WithDefault (Expression "in: in"))
                        ]
        it "adds lambda nodeid to node mapping" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                Graph.withGraph (top |> u1) $ use $ breadcrumbHierarchy . BH.children
            withResult res $ \(toList -> mapping) -> do
                let isLambdaNode n = case n ^. BH.self of
                        AnonymousNode _ -> True
                        _               -> False
                    lambdaNodes = filter isLambdaNode mapping
                lambdaNodes `shouldSatisfy` (not . null)
        it "puts + inside plus lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "a: b: a + b" def
                Graph.getNodes loc'
            withResult res $ \(excludeEdges -> nodes) -> do
                nodes `shouldSatisfy` ((== 1) . length)
                head nodes `shouldSatisfy` (\a -> a ^. Node.nodeType . Node.expression == "a + b")
        it "places connections between + node and output" $ \env -> do
          u1 <- mkUUID
          res <- evalEmp env $ do
              let loc' = top |> u1
              Graph.addNode top u1 "a: b: a + b" def
              Graph.getConnections loc'
          withResult res $ \conns -> do
              -- one from a to +, one from b to + and one from + to output edge
              conns `shouldSatisfy` ((== 3) . length)
        it "cleans after removing `foo = a: a` with `4` inside connected to output" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef u2 Port.All, InPortRef out (Port.Arg 0))
                uncurry (connectToInput loc') referenceConnection
                Graph.removeNodes top [u1]
                Graph.withGraph top $ (,,) <$> use (breadcrumbHierarchy .  BH.children) <*> runASTOp exprs <*> runASTOp links
            withResult res $ \(mapping, edges, nodes) -> do
                mapping `shouldSatisfy` Map.null
                edges   `shouldSatisfy` null
                nodes   `shouldSatisfy` null
        it "removes `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.removeNodes top [u1]
                Graph.withGraph top $ (,,) <$> use (breadcrumbHierarchy . BH.children) <*> runASTOp exprs <*> runASTOp links
            withResult res $ \(mapping, edges, nodes) -> do
                mapping `shouldSatisfy` Map.null
                edges   `shouldSatisfy` null
                nodes   `shouldSatisfy` null
        it "RHS of `foo = a: a` is Lam" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.withGraph top $ runASTOp $ do
                    isLambda <- ASTRead.rhsIsLambda =<< ASTRead.getASTPointer u1
                    isTrivial <- AST.isTrivialLambda =<< ASTRead.getASTTarget u1
                    return $ isLambda && isTrivial
            withResult res $ \a -> a `shouldBe` True
        it "changes name of a variable in-place" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo" def
                Graph.renameNode top u1 "bar"
                Graph.getNodes top
            withResult res $ \nodes -> head nodes ^. Node.name `shouldBe` "bar"
        it "retains connection after node rename" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.addNode top u2 "id node1" def
                Graph.renameNode top u1 "node5"
                graph <- Graph.getGraph top
                expression <- Graph.withGraph top $ runASTOp $ do
                    target <- ASTRead.getASTTarget u2
                    printExpression target
                return (graph, expression)
            withResult res $ \(graph, expression) -> do
                let nodes = graph ^. Graph.nodes
                    Just four = find (\n -> n ^. Node.nodeId == u1) nodes
                four ^. Node.name `shouldBe` "node5"
                expression `shouldBe` "id node5"
                let connections = graph ^. Graph.connections
                connections `shouldMatchList` [(OutPortRef u1 Port.All, InPortRef u2 (Port.Arg 0))]
        it "changes node expression" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "‹0›123" def
                node  <- Graph.setNodeExpression top u1 "‹0›456"
                nodes <- Graph.getNodes top
                return (node, nodes)
            withResult res $ \(node, nodes) -> do
                node ^. Node.nodeType `shouldBe` Node.ExpressionNode "456"
                node ^. Node.nodeId `shouldBe` u1
                nodes `shouldSatisfy` ((== 1) . length)
        it "changes expression to lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                node  <- Graph.setNodeExpression top u1 "a: a"
                nodes <- Graph.getNodes top
                return (node, nodes)
            withResult res $ \(node, nodes) -> do
                node ^. Node.nodeType `shouldBe` Node.ExpressionNode "a: a"
                node ^. Node.nodeId `shouldBe` u1
                node ^. Node.canEnter `shouldBe` True
                nodes `shouldSatisfy` ((== 1) . length)
        it "does not allow to change expression to assignment" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.setNodeExpression top u1 "foo = a: a"
            case res of
                Left err -> err `shouldStartWith` "ParserException"
                Right _  -> expectationFailure "should throw ParserException"
        it "changes expression to lambda with node inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.setNodeExpression top u1 "a: b: a + b"
                Graph.getGraph (top |> u1)
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ = graph
                let [node] = excludeEdges nodes
                node ^. Node.nodeType `shouldBe` Node.ExpressionNode "a + b"
                connections `shouldSatisfy` ((== 3) . length)
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
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Self))
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
                connectToInput top (OutPortRef u1 Port.All) (InPortRef u2 (Port.Arg 0))
                Graph.addNode top u3 "bar" def
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u3 Port.Self)
                Graph.addNode top u4 "baz" def
                connectToInput top (OutPortRef u3 Port.All) (InPortRef u4 Port.Self)
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u4
                    (,) <$> ASTRead.getASTVar u3 <*> ASTDeconstruct.dumpAccessors accs
            withResult res $ \(reference, (node, accs)) -> do
                node `shouldBe` Just reference
                accs `shouldBe` ["baz"]
    describe "show ports on not-yet-typechecked nodes" $ do
        it "shows two input ports on +" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                Graph.getNodes top
            withResult res $ \[plus] -> do
                inputPorts plus `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    ]
        xit "shows self & one input port on succ" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "succ" def
                Graph.getNodes top
            withResult res $ \[succ'] -> do
                inputPorts succ' `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "succ"))
                    ]
        it "connects to input port on +" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                Graph.addNode top u2 "1" def
                Graph.getNodes top
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                Graph.getNodes top
            withResult res $ \nodes -> do
                let Just plus = find (\a -> view Node.nodeId a == u1) nodes
                inputPorts plus `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    ]
        xit "connects to more than one port" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                Graph.addNode top u3 "2" def
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                connectToInput top (OutPortRef u3 Port.All) (InPortRef u1 (Port.Arg 1))
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                inputPorts n `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self"  TStar (Port.WithDefault (Expression "func"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar Port.Connected
                    ]
        xit "connects five nodes to func" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            u5 <- mkUUID
            u6 <- mkUUID
            u7 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                Graph.addNode top u3 "2" def
                Graph.addNode top u4 "3" def
                Graph.addNode top u5 "4" def
                Graph.addNode top u6 "5" def
                Graph.addNode top u7 "6" def
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                connectToInput top (OutPortRef u3 Port.All) (InPortRef u1 (Port.Arg 1))
                connectToInput top (OutPortRef u4 Port.All) (InPortRef u1 (Port.Arg 2))
                connectToInput top (OutPortRef u5 Port.All) (InPortRef u1 (Port.Arg 3))
                connectToInput top (OutPortRef u6 Port.All) (InPortRef u1 (Port.Arg 4))
                connectToInput top (OutPortRef u7 Port.All) (InPortRef u1 (Port.Arg 5))
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                inputPorts n `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "func"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "arg2" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 3)) "arg3" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 4)) "arg4" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 5)) "arg5" TStar Port.Connected
                    ]
        xit "removes empty port on disconnect" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 0))
                Graph.disconnect top (InPortRef u1 (Port.Arg 0))
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                inputPorts n `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self"  TStar (Port.WithDefault (Expression "func"))
                    ]
        xit "disconnect first connection when two nodes connected" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "first" def
                Graph.addNode top u2 "second" def
                Graph.addNode top u3 "func" def
                connectToInput top (OutPortRef u1 Port.All) (InPortRef u3 (Port.Arg 0))
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u3 (Port.Arg 1))
                Graph.disconnect top (InPortRef u3 (Port.Arg 0))
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u3
            withResult res $ \n -> do
                inputPorts n `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "func"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar Port.Connected
                    ]
        xit "disconnects first connection when three connected" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "first" def
                Graph.addNode top u2 "second" def
                Graph.addNode top u3 "third" def
                Graph.addNode top u4 "func" def
                connectToInput top (OutPortRef u1 Port.All) (InPortRef u4 (Port.Arg 0))
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u4 (Port.Arg 1))
                connectToInput top (OutPortRef u3 Port.All) (InPortRef u4 (Port.Arg 2))
                Graph.disconnect top (InPortRef u4 (Port.Arg 0))
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u4
            withResult res $ \n -> do
                inputPorts n `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "func"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "arg2" TStar Port.Connected
                    ]
    describe "port manipulation" $ do
        let buildInputEdge' loc nid = Graph.withGraph loc $ runASTOp $ GraphBuilder.buildConnections >>= \c -> GraphBuilder.buildInputEdge c nid
        it "adds port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                let referenceConnection = (OutPortRef input (Port.Projection 0), InPortRef output (Port.Arg 0))
                return (inputEdge, defFoo, connections, referenceConnection)
            withResult res $ \(inputEdge, defFoo, connections, referenceConnection) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.NotConnected
                    ]
                inputPorts defFoo `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    ]
                connections `shouldMatchList` [referenceConnection]
        it "adds port at the first position" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 0
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                let referenceConnection = (OutPortRef input (Port.Projection 1), InPortRef output (Port.Arg 0))
                return (inputEdge, defFoo, connections, referenceConnection)
            withResult res $ \(inputEdge, defFoo, connections, referenceConnection) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "b" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "a" TStar Port.Connected
                    ]
                inputPorts defFoo `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "b" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "a" TStar Port.NotConnected
                    ]
                connections `shouldMatchList` [referenceConnection]
        it "adds two ports" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                Graph.addPort loc' input 2
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                return (inputEdge, defFoo)
            withResult res $ \(inputEdge, defFoo) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "c" TStar Port.NotConnected
                    ]
                inputPorts defFoo `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "c" TStar Port.NotConnected
                    ]
        it "adds port on literal lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 2
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                return (inputEdge, defFoo, connections)
            withResult res $ \(inputEdge, defFoo, connections) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "c" TStar Port.NotConnected
                    ]
                inputPorts defFoo `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "c" TStar Port.NotConnected
                    ]
                connections `shouldSatisfy` ((== 3) . length)
        it "connects to added port" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode top u2 "func" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                connectToInput top (OutPortRef u2 Port.All) (InPortRef u1 (Port.Arg 1))
                node <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections top
                return (node, connections)
            withResult res $ \(node, connections) -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.Connected
                    ]
                connections `shouldMatchList` [
                      (OutPortRef u2 Port.All, InPortRef u1 (Port.Arg 1))
                    ]
        it "removes port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (OutPortRef' (OutPortRef input (Port.Projection 1)))
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                return (inputEdge, defFoo)
            withResult res $ \(inputEdge, node) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    ]
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    ]
        it "renames port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.renamePort loc' (OutPortRef' (OutPortRef input (Port.Projection 0))) "foo"
                Graph.renamePort loc' (OutPortRef' (OutPortRef input (Port.Projection 1))) "bar"
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "foo" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "bar" TStar Port.NotConnected
                    ]
        it "changes ports order" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: c: d: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (OutPortRef' (OutPortRef input (Port.Projection 0))) 2
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "b" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "c" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "a" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 3)) "d" TStar Port.NotConnected
                    ]
        it "connects to added port inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "func" def
                Just (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                connectToInput loc' (OutPortRef input (Port.Projection 1)) (InPortRef u2 (Port.Arg 0))
                inputEdge <- buildInputEdge' loc' input
                connections <- Graph.getConnections loc'
                let referenceConnections = [
                        (OutPortRef input (Port.Projection 0), InPortRef output (Port.Arg 0))
                      , (OutPortRef input (Port.Projection 1), InPortRef u2 (Port.Arg 0))
                      ]
                return (inputEdge, connections, referenceConnections)
            withResult res $ \(inputEdge, connections, referenceConnections) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.Connected
                    ]
                connections `shouldMatchList` referenceConnections
        it "does not allow to remove All port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.removePort loc' (OutPortRef' (OutPortRef input Port.All))
            case res of
                Right _ -> expectationFailure "should throw exception"
                Left err -> err `shouldStartWith` "CannotRemovePortException"
        it "removes port that is connected inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "func" def
                Just (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                connectToInput loc' (OutPortRef input (Port.Projection 1)) (InPortRef u2 (Port.Arg 0))
                Graph.removePort loc' (OutPortRef' (OutPortRef input (Port.Projection 1)))
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                nodeIds <- (map (^. Node.nodeId)) <$> (Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildNodes)
                let referenceConnections = [(OutPortRef input (Port.Projection 0), InPortRef output (Port.Arg 0))]
                return (inputEdge, defFoo, connections, referenceConnections, nodeIds)
            withResult res $ \(inputEdge, node, connections, referenceConnections, nodeIds) -> do
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                    ]
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    ]
                connections `shouldMatchList` referenceConnections
                nodeIds `shouldMatchList` [u2]
        it "can build input edge after connecting input edge to self" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |> u1
                Just (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' input 1
                Graph.addNode loc' u2 "succ" def
                connectToInput loc' (OutPortRef input (Port.Projection 1)) (InPortRef u2 Port.Self)
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
              outputPorts inputEdge `shouldMatchList` [
                    Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.Connected
                  , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.Connected
                  ]
    describe "node sequence" $ do
        it "adds one node to sequence" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.getNodeIdSequence top
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u1]
        it "adds three nodes in line" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (10, 10) False
                Graph.addNode top u2 "2" $ NodeMeta (10, 20) False
                Graph.addNode top u3 "3" $ NodeMeta (10, 30) False
                Graph.getNodeIdSequence top
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u1, u2, u3]
        it "adds three nodes in reverse order" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (10, 30) False
                Graph.addNode top u2 "2" $ NodeMeta (10, 20) False
                Graph.addNode top u3 "3" $ NodeMeta (10, 10) False
                Graph.getNodeIdSequence top
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u2, u1]
        it "updates sequence after node removal" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (10, 30) False
                Graph.addNode top u2 "2" $ NodeMeta (10, 20) False
                Graph.addNode top u3 "3" $ NodeMeta (10, 10) False
                Graph.removeNodes top [u2]
                Graph.getNodeIdSequence top
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u1]
        it "updates sequence after node meta update" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (10, 10) False
                Graph.addNode top u2 "2" $ NodeMeta (10, 20) False
                Graph.addNode top u3 "3" $ NodeMeta (10, 30) False
                Graph.setNodeMeta top u3 $ NodeMeta (10, 10) False
                Graph.setNodeMeta top u2 $ NodeMeta (20, 30) False
                Graph.setNodeMeta top u1 $ NodeMeta (30, 20) False
                Graph.getNodeIdSequence top
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u2, u1]
        it "adds one node inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc = top |> u1
                Graph.addNode loc u2 "4" def
                Graph.getNodeIdSequence loc
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` [u2]
        it "adds two nodes inside" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc = top |> u1
                Graph.addNode loc u2 "4" $ NodeMeta (10, 10) False
                Graph.addNode loc u3 "6" $ NodeMeta (10, 20) False
                Graph.getNodeIdSequence loc
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` [u2, u3]
        it "adds one node inside and removes it" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: a" def
                let loc = top |> u1
                Graph.addNode loc u2 "1" def
                Graph.removeNodes loc [u2]
                Graph.getNodeIdSequence loc
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` []
    describe "pattern match" $ do
        it "connects two outputs when one of them is pattern match" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                Graph.addNode top u2 "Vector x y z" def
                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))
                Graph.withGraph top $ runASTOp $
                    (,,,) <$> GraphBuilder.buildNode u1
                          <*> GraphBuilder.buildNode u2
                          <*> GraphBuilder.buildConnections
                          <*> (ASTRead.getASTPointer u2 >>= ASTRead.varIsPatternMatch)
            withResult res $ \(myVec, pattern, connections, isPatternMatch) -> do
                myVec ^. Node.name `shouldBe` "node1"
                pattern ^. Node.name `shouldBe` "Vector x y z"
                outputPorts pattern `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "x" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "y" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "z" TStar Port.NotConnected
                    ]
                inputPorts pattern `shouldMatchList` [
                    -- FIXME[MM]: this should be connected
                      Port.Port (Port.InPortId (Port.Arg 0)) "Input" TStar (Port.WithDefault (Expression "Vector x y z"))
                    ]
                isPatternMatch `shouldBe` True
                connections `shouldMatchList` [(OutPortRef u1 Port.All, InPortRef u2 (Port.Arg 0))]
        it "disconnects pattern match" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                Graph.addNode top u2 "Vector x y z" def
                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))
                Graph.disconnect top (InPortRef u2 (Port.Arg 0))

                Graph.withGraph top $ runASTOp $
                    (,,,) <$> GraphBuilder.buildNode u1
                          <*> GraphBuilder.buildNode u2
                          <*> GraphBuilder.buildConnections
                          <*> (ASTRead.getASTPointer u2 >>= ASTRead.varIsPatternMatch)
            withResult res $ \(myVec, pattern, connections, isPatternMatch) -> do
                myVec   ^. Node.name `shouldBe` "node1"
                pattern ^. Node.name `shouldBe` "Vector x y z"
                pattern ^. Node.nodeType `shouldBe` Node.ExpressionNode "Nothing"
                outputPorts pattern `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "x" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "y" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "z" TStar Port.NotConnected
                    ]
                inputPorts pattern `shouldMatchList` [
                    -- FIXME[MM]: this should be NotConnected
                      Port.Port (Port.InPortId (Port.Arg 0)) "Input" TStar (Port.WithDefault (Expression "Vector x y z"))
                    ]
                isPatternMatch `shouldBe` True
                connections `shouldMatchList` []
        it "connects to pattern match, disconnects and connects again" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                Graph.addNode top u2 "Vector x y z" def
                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))
                Graph.disconnect top (InPortRef u2 (Port.Arg 0))
                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))

                Graph.withGraph top $ runASTOp $
                    (,,,) <$> GraphBuilder.buildNode u1
                          <*> GraphBuilder.buildNode u2
                          <*> GraphBuilder.buildConnections
                          <*> (ASTRead.getASTPointer u2 >>= ASTRead.varIsPatternMatch)
            withResult res $ \(myVec, pattern, connections, isPatternMatch) -> do
                myVec ^. Node.name `shouldBe` "node1"
                pattern ^. Node.name `shouldBe` "Vector x y z"
                outputPorts pattern `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "x" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "y" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "z" TStar Port.NotConnected
                    ]
                inputPorts pattern `shouldMatchList` [
                    -- FIXME[MM]: this should be connected
                      Port.Port (Port.InPortId (Port.Arg 0)) "Input" TStar (Port.WithDefault (Expression "Vector x y z"))
                    ]
                isPatternMatch `shouldBe` True
                connections `shouldMatchList` [(OutPortRef u1 Port.All, InPortRef u2 (Port.Arg 0))]
        it "connects deconstructed value to other nodes" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                Graph.addNode top u2 "Vector x y z" def
                Graph.addNode top u3 "a: b: a + b" def

                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))
                connectToInput top (OutPortRef u2 (Port.Projection 0)) (InPortRef u3 (Port.Arg 0))
                connectToInput top (OutPortRef u2 (Port.Projection 1)) (InPortRef u3 (Port.Arg 1))

                Graph.withGraph top $ runASTOp $
                    (,,) <$> GraphBuilder.buildNode u2
                         <*> GraphBuilder.buildNode u3
                         <*> GraphBuilder.buildConnections
            withResult res $ \(pattern, plus, connections) -> do
                outputPorts pattern `shouldMatchList` [
                    -- FIXME[MM]: these should be connected
                      Port.Port (Port.OutPortId (Port.Projection 0)) "x" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "y" TStar Port.NotConnected
                    -------------------
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "z" TStar Port.NotConnected
                    ]
                inputPorts pattern `shouldMatchList` [
                      -- FIXME[MM]: this should be connected
                      Port.Port (Port.InPortId (Port.Arg 0)) "Input" TStar (Port.WithDefault (Expression "Vector x y z"))
                    ]

                -- outputPorts plus `shouldMatchList` [
                --       Port.Port (Port.OutPortId Port.All) "Output" TStar (Port.WithDefault (Expression "x + y"))
                --     ]
                inputPorts plus `shouldMatchList` [
                        Port.Port (Port.InPortId (Port.Arg 0)) "a"    TStar Port.Connected
                      , Port.Port (Port.InPortId (Port.Arg 1)) "b"    TStar Port.Connected
                    ]
                connections `shouldMatchList` [
                      (OutPortRef u1 Port.All, InPortRef u2 (Port.Arg 0))
                    , (OutPortRef u2 (Port.Projection 0), InPortRef u3 (Port.Arg 0))
                    , (OutPortRef u2 (Port.Projection 1), InPortRef u3 (Port.Arg 1))
                    ]
        it "connects two outputs when one of them is nested pattern match with literals" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                Graph.addNode top u2 "SomeCons (Just a) 0 \"foo\" x" def
                Graph.connect top (OutPortRef u1 Port.All) (OutPortRef' (OutPortRef u2 Port.All))

                Graph.withGraph top $ runASTOp $
                    (,,,) <$> GraphBuilder.buildNode u1
                          <*> GraphBuilder.buildNode u2
                          <*> GraphBuilder.buildConnections
                          <*> (ASTRead.getASTPointer u2 >>= ASTRead.varIsPatternMatch)
            withResult res $ \(myVec, pattern, connections, isPatternMatch) -> do
                myVec ^. Node.name `shouldBe` "node1"
                pattern ^. Node.name `shouldBe` "SomeCons (Just a) 0 \"foo\" x"
                outputPorts pattern `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "x" TStar Port.NotConnected
                    ]
                inputPorts pattern `shouldMatchList` [
                    -- FIXME[MM]: this should be connected
                      Port.Port (Port.InPortId (Port.Arg 0)) "Input" TStar (Port.WithDefault (Expression "SomeCons (Just a) 0 \"foo\" x"))
                    ]
                isPatternMatch `shouldBe` True
                connections `shouldMatchList` [(OutPortRef u1 Port.All, InPortRef u2 (Port.Arg 0))]
        it "supports lambdas pattern matching on their argument" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "(Foobar a b c): b" def
                node <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                let loc' = top |> u1
                graph <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildGraph
                Just (input, output) <- Graph.withGraph loc' $ runASTOp $ do
                    c <- GraphBuilder.buildConnections
                    GraphBuilder.buildEdgeNodes c
                return (node, graph, input, output)
            withResult res $ \(node, graph, input, output) -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.NotConnected
                    ]
                let Graph.Graph nodes connections _ = graph
                excludeEdges nodes `shouldBe` []
                outputPorts input `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "c" TStar Port.NotConnected
                    ]

                inputPorts output `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.Connected
                    ]

                connections `shouldMatchList` [
                      (OutPortRef (input ^. Node.nodeId) (Port.Projection 1), InPortRef (output ^. Node.nodeId) (Port.Arg 0))
                    ]
        it "supports multi-parameter lambdas pattern matching on their arguments" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "(Foobar a b (Just c)): x: (Quux y z): c" def
                node <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                let loc' = top |> u1
                graph <- Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildGraph
                Just (input, output) <- Graph.withGraph loc' $ runASTOp $ do
                    c <- GraphBuilder.buildConnections
                    GraphBuilder.buildEdgeNodes c
                return (node, graph, input, output)
            withResult res $ \(node, graph, input, output) -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "x"    TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "arg2" TStar Port.NotConnected
                    ]
                let Graph.Graph nodes connections _ = graph
                excludeEdges nodes `shouldBe` []
                outputPorts input `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "c" TStar Port.Connected
                    , Port.Port (Port.OutPortId (Port.Projection 3)) "x" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 4)) "y" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 5)) "z" TStar Port.NotConnected
                    ]

                inputPorts output `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.Connected
                    ]

                connections `shouldMatchList` [
                      (OutPortRef (input ^. Node.nodeId) (Port.Projection 2), InPortRef (output ^. Node.nodeId) (Port.Arg 0))
                    ]
