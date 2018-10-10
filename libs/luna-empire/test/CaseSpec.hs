{-# LANGUAGE OverloadedStrings #-}

module CaseSpec (spec) where

import           Control.Lens                    ((^..))
import           Data.Foldable                   (toList)
import           Data.List                       (find, stripPrefix)
import qualified Data.Map                        as Map
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import qualified Empire.ASTOps.Parse             as Parser
import           Empire.ASTOps.Print             (printExpression)
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST (isTrivialLambda)
import qualified Empire.Commands.Graph           as Graph (addNode, addPort, connect, disconnect, getConnections, getGraph, getNodes,
                                                           movePort, removeNodes, removePort, renameNode, renamePort, withGraph)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import           Empire.Commands.Library         (withLibrary)
import qualified Empire.Commands.Typecheck       as Typecheck (run)
import           Empire.Data.BreadcrumbHierarchy (BreadcrumbDoesNotExistException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (breadcrumbHierarchy)
import qualified Empire.Data.Graph               as Graph (breadcrumbHierarchy)
import qualified Empire.Data.Library             as Library (body)
import           Empire.Empire                   (InterpreterEnv (..))
import           LunaStudio.Data.Connection      (Connection (..))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..), (|>|), (|>-))
import           LunaStudio.Data.LabeledTree     (LabeledTree (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import           LunaStudio.Data.Port            (OutPortTree (..))
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (Constant, Expression))
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.TypeRep         (TypeRep (TCons, TLam, TStar, TVar))
import           Prologue                        hiding (mapping, toList, (|>))
-- import           OCI.IR.Class                    (exprs, links)

import           Test.Hspec                      (Selector, Spec, around, describe, expectationFailure, it, parallel, shouldBe,
                                                  shouldContain, shouldMatchList, shouldSatisfy, shouldStartWith, shouldThrow, xdescribe,
                                                  xit)

import           EmpireUtils


spec :: Spec
spec = around withChannels $ parallel $ do
    describe "case" $ do
        xit "creates case node" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "myVec" def
                node <- Graph.addNode top u2 "case (node1) of Vector x y z: y" def
                return node
            withResult res $ \node -> do
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port [Port.Arg 0] "arg0" TStar Port.Connected
                    , Port.Port [Port.Arg 1] "arg1" TStar Port.NotConnected
                    ]
        it "shows anonymous breadcrumb in map (x:x)" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                node  <- Graph.addNode top u1 "map x:x" def
                graph <- Graph.getGraph $ top |>- (u1, 0)
                return (node, graph)
            withResult res $ \(node, graph) -> do
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "map x: x")
                    , Port.Port [Port.Self]  "self" TStar Port.NotConnected
                    , Port.Port [Port.Arg 0] "arg0" TStar (Port.WithDefault (Expression "x: x"))
                    ]
                let Graph.Graph nodes connections (Just inputEdge) (Just outputEdge) _ _ = graph
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "x" TStar Port.NotConnected)
                    ]
                (outputEdge ^.. Node.outputEdgePorts . traverse) `shouldMatchList` [
                      Port.Port [] "output" TStar Port.Connected
                    ]
                connections `shouldMatchList` [
                      Connection (OutPortRef (NodeLoc def $ inputEdge  ^. Node.nodeId) [Port.Projection 0])
                       (InPortRef  (NodeLoc def $ outputEdge ^. Node.nodeId) [])
                    ]
        {-xit "shows anonymous breadcrumbs in foo ((Acc a): b: a + b) 1 ((Vector a b c): a * b + c)" $ \env -> do-}
            {-u1 <- mkUUID-}
            {-res <- evalEmp env $ do-}
                {-node <- Graph.addNode top u1 "foo ((Acc a): b: a + b) 1 ((Vector a b c): a * b + c)" def-}
                {-graph0 <- Graph.getGraph $ top |>- (u1, 0)-}
                {-graph2 <- Graph.getGraph $ top |>- (u1, 2)-}
                {-return (node, graph0, graph2)-}
            {-withResult res $ \(node, graph0, graph2) -> do-}
                {-inputPorts node `shouldMatchList` [-}
                      {-Port.Port (Port.InPortId Port.Self)    "self" TStar (Port.WithDefault (Expression "foo"))-}
                    {-, Port.Port (Port.InPortId (Port.Arg 0)) "arg0" TStar (Port.WithDefault (Expression "((Acc a): b: a + b)"))-}
                    {-, Port.Port (Port.InPortId (Port.Arg 1)) "arg1" TStar (Port.WithDefault (Constant (IntValue 1)))-}
                    {-, Port.Port (Port.InPortId (Port.Arg 2)) "arg2" TStar (Port.WithDefault (Expression "((Vector a b c): a * b + c)"))-}
                    {-]-}
                {-let Graph.Graph nodes connections (Just inputEdge) (Just outputEdge) _ = graph0-}
                {-outputPorts inputEdge `shouldMatchList` [-}
                    {---FIXME[MM]: all ports in this test should be connected-}
                      {-Port.Port (Port.OutPortId (Port.Projection 0 [])) "a" TStar Port.NotConnected-}
                    {-, Port.Port (Port.OutPortId (Port.Projection 1 [])) "b" TStar Port.NotConnected-}
                    {-]-}
                {-inputPorts outputEdge `shouldMatchList` [-}
                      {-Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.NotConnected-}
                    {-]-}
                {--- from input ports to + and from + to output-}
                {-connections `shouldSatisfy` ((== 3) . length)-}
                {-let Graph.Graph nodes connections (Just inputEdge) (Just outputEdge) _ = graph2-}
                {-(inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [-}
                    {---FIXME[MM]: all ports in this test should be connected-}
                      {-OutPortTree (Port.Port (Port.OutPortId []) "a" TStar Port.NotConnected) []-}
                    {-, OutPortTree (Port.Port (Port.OutPortId []) "b" TStar Port.NotConnected) []-}
                    {-, OutPortTree (Port.Port (Port.OutPortId []) "c" TStar Port.NotConnected) []-}
                    {-]-}
                {-(outputEdge ^.. Node.outputEdgePorts . traverse) `shouldMatchList` [-}
                      {-Port.Port (Port.InPortId (Port.Arg 0)) "output" TStar Port.NotConnected-}
                    {-]-}
                {--- connections `shouldMatchList` [-}
                {---       (OutPortRef (inputEdge ^. Node.nodeId) (Port.Projection 0),-}
                {---       InPortRef (outputEdge ^. Node.nodeId) (Port.Arg 0))-}
                {---     ]-}
        it "cannot enter map node in map (x:x)" $ \env -> do
            u1 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "map x:x" def
                    Graph.getGraph (top |>| u1)
            let breadcrumbException :: Selector BreadcrumbDoesNotExistException
                breadcrumbException = const True
            res `shouldThrow` breadcrumbException
