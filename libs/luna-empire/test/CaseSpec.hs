{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module CaseSpec (spec) where

import           Data.Foldable                 (toList)
import           Data.List                     (find, stripPrefix)
import qualified Data.Map                      as Map
import qualified Empire.API.Data.Graph         as Graph
import qualified Empire.Data.Graph             as Graph (breadcrumbHierarchy)
import           Empire.API.Data.GraphLocation (GraphLocation(..))
import qualified Empire.API.Data.Node          as Node (NodeType(ExpressionNode, InputEdge, OutputEdge),
                                                        canEnter, expression, name, nodeId, nodeType, ports)
import           Empire.API.Data.NodeMeta      (NodeMeta(..))
import qualified Empire.API.Data.Port          as Port
import           Empire.API.Data.PortDefault   (PortDefault (Constant, Expression), Value(IntValue))
import           Empire.API.Data.PortRef       (InPortRef (..), OutPortRef (..), AnyPortRef(..))
import           Empire.API.Data.TypeRep       (TypeRep(TCons, TStar, TLam, TVar))
import           Empire.ASTOp                  (runASTOp)
import qualified Empire.ASTOps.Deconstruct     as ASTDeconstruct
import qualified Empire.ASTOps.Parse           as Parser
import           Empire.ASTOps.Print           (printExpression)
import qualified Empire.ASTOps.Read            as ASTRead
import qualified Empire.Commands.AST           as AST (isTrivialLambda, dumpGraphViz)
import qualified Empire.Commands.Graph         as Graph (addNode, connect, getGraph, getNodes,
                                                         getConnections, getNodeIdSequence, removeNodes, withGraph,
                                                         renameNode, disconnect, addPort, movePort,
                                                         removePort, renamePort)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import           Empire.Commands.Library         (withLibrary)
import qualified Empire.Commands.Typecheck       as Typecheck (run)
import           Empire.Data.Graph               (breadcrumbHierarchy)
import qualified Empire.Data.Library             as Library (body)
import           Empire.Data.BreadcrumbHierarchy (NodeIDTarget (..))
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Empire                   (InterpreterEnv(..))
import           Prologue                        hiding (mapping, toList, (|>))
import           OCI.IR.Class                    (exprs, links)

import           Test.Hspec (Spec, around, describe, expectationFailure, it, parallel,
                             shouldBe, shouldContain, shouldSatisfy, shouldMatchList,
                             shouldStartWith, xit, xdescribe)

import           EmpireUtils


spec :: Spec
spec = around withChannels $ id $ do
    describe "case" $ do
        xit "creates case node" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                node <- Graph.addNode top u2 "case (node1) of Vector x y z: y" def
                return node
            withResult res $ \node -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar Port.Connected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar Port.NotConnected
                    ]
        it "shows anonymous breadcrumb in map (x:x)" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                node  <- Graph.addNode top u1 "map x:x" def
                graph <- Graph.getGraph $ top |>- (u1, 0)
                return (node, graph)
            withResult res $ \(node, graph) -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "map"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar (Port.WithDefault (Expression "x: x"))
                    ]
                let Graph.Graph nodes connections _ = graph
                    Just inputEdge = find (\n -> n ^. Node.nodeType == Node.InputEdge) nodes
                    Just outputEdge = find (\n -> n ^. Node.nodeType == Node.OutputEdge) nodes
                outputPorts inputEdge `shouldMatchList` [
                      Port.Port (Port.OutPortId (Port.Projection 0)) "x" TStar Port.Connected
                    ]
                inputPorts outputEdge `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.Connected
                    ]
                connections `shouldMatchList` [
                      (OutPortRef (inputEdge ^. Node.nodeId) (Port.Projection 0),
                      InPortRef (outputEdge ^. Node.nodeId) (Port.Arg 0))
                    ]
        xit "shows anonymous breadcrumbs in foo ((Acc a): b: a + b) 1 ((Vector a b c): a * b + c)" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                node <- Graph.addNode top u1 "foo ((Acc a): b: a + b) 1 ((Vector a b c): a * b + c)" def
                graph0 <- Graph.getGraph $ top |>- (u1, 0)
                graph2 <- Graph.getGraph $ top |>- (u1, 2)
                return (node, graph0, graph2)
            withResult res $ \(node, graph0, graph2) -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "foo"))
                    , Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar (Port.WithDefault (Expression "((Acc a): b: a + b)"))
                    , Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar (Port.WithDefault (Constant (IntValue 1)))
                    , Port.Port (Port.InPortId (Port.Arg 2)) "arg2" TStar (Port.WithDefault (Expression "((Vector a b c): a * b + c)"))
                    ]
                let Graph.Graph nodes connections _ = graph0
                    Just inputEdge = find (\n -> n ^. Node.nodeType == Node.InputEdge) nodes
                    Just outputEdge = find (\n -> n ^. Node.nodeType == Node.OutputEdge) nodes
                outputPorts inputEdge `shouldMatchList` [
                    --FIXME[MM]: all ports in this test should be connected
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.NotConnected
                    ]
                inputPorts outputEdge `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.NotConnected
                    ]
                -- from input ports to + and from + to output
                connections `shouldSatisfy` ((== 3) . length)
                let Graph.Graph nodes connections _ = graph2
                    Just inputEdge = find (\n -> n ^. Node.nodeType == Node.InputEdge) nodes
                    Just outputEdge = find (\n -> n ^. Node.nodeType == Node.OutputEdge) nodes
                outputPorts inputEdge `shouldMatchList` [
                    --FIXME[MM]: all ports in this test should be connected
                      Port.Port (Port.OutPortId (Port.Projection 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.OutPortId (Port.Projection 2)) "c" TStar Port.NotConnected
                    ]
                inputPorts outputEdge `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.NotConnected
                    ]
                -- connections `shouldMatchList` [
                --       (OutPortRef (inputEdge ^. Node.nodeId) (Port.Projection 0),
                --       InPortRef (outputEdge ^. Node.nodeId) (Port.Arg 0))
                --     ]
        it "cannot enter map node in map (x:x)" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "map x:x" def
                Graph.getGraph (top |> u1)
            case res of
                Left err -> case stripPrefix "Breadcrumb" err of
                    Just _ -> return ()
                    _      -> expectationFailure err
                Right _  -> expectationFailure "should throw"
        xit "lambda with compound expression has more than one node inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "(Vector a b c): a * b + c" def
                Graph.getGraph (top |> u1)
            withResult res $ \(Graph.Graph nodes connections _) -> do
                excludeEdges nodes `shouldSatisfy` ((== 2) . length)
        xit "compound expression has as many nodes as variables" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ Graph.addNode top u1 "a * b + c" def
            withResult res $ \node -> do
                inputPorts node `shouldMatchList` [
                      Port.Port (Port.InPortId (Port.Arg 0)) "a" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 1)) "b" TStar Port.NotConnected
                    , Port.Port (Port.InPortId (Port.Arg 2)) "c" TStar Port.NotConnected
                    ]
                outputPorts node `shouldMatchList` [
                      Port.Port (Port.OutPortId Port.All) "output" TStar Port.NotConnected
                    ]