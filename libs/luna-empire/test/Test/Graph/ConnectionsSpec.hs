module Test.Graph.ConnectionsSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified LunaStudio.Data.Graph        as Graph
import qualified LunaStudio.Data.Node         as Node

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.GraphLocation  ((|>|))
import LunaStudio.Data.Port           (InPortIndex (Arg, Self), InPorts (InPorts),
                                       LabeledTree (LabeledTree),
                                       Port (Port), PortState (WithDefault))
import LunaStudio.Data.PortDefault    (PortDefault (Expression))
import LunaStudio.Data.PortRef        (AnyPortRef (InPortRef'))
import LunaStudio.Data.TypeRep        (TypeRep (TCons, TLam, TVar))
import Test.Hspec                     (Spec, it)
import Test.Hspec.Empire              (findNodeByName, findNodeIdByName,
                                       inPortRef, noAction, outPortRef,
                                       runTests, testCase, testCaseWithTC)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList)
import Text.RawString.QQ              (r)

spec :: Spec
spec = runTests "connections tests" $ do
    it "shows connection for variable used in argument in lambda" $ let
        code = [r|
            def main:
                list1 = [1,2]
                node = [1].filter (x: list1)
                None
            |]
        expectedConnections list1Id nodeId =
            [ (outPortRef list1Id mempty, inPortRef nodeId [Arg 0]) ]
        prepare gl = do
            Just list1Id         <- findNodeIdByName gl "list1"
            Just nodeId          <- findNodeIdByName gl "node"
            Graph.withGraph gl . runASTOp $ (list1Id, nodeId, )
                <$> GraphBuilder.buildConnections
        in testCase code code $ \gl -> do
            (list1Id, nodeId, connections) <- prepare gl
            connections `shouldMatchList` expectedConnections list1Id nodeId
    it "shows connection for variable deeply used in argument in lambda" $ let
        code = [r|
            def main:
                list1 = [1,2]
                node = [1].filter (x: x == list1.head.get)
                None
            |]
        expectedConnections list1Id nodeId =
            [ (outPortRef list1Id mempty, inPortRef nodeId [Arg 0]) ]
        prepare gl = do
            Just list1Id         <- findNodeIdByName gl "list1"
            Just nodeId          <- findNodeIdByName gl "node"
            Graph.withGraph gl . runASTOp $ (list1Id, nodeId, )
                <$> GraphBuilder.buildConnections
        in testCase code code $ \gl -> do
            (list1Id, nodeId, connections) <- prepare gl
            connections `shouldMatchList` expectedConnections list1Id nodeId
    it "does not show connection for accessor named identically as node" $ let
        code = [r|
            def main:
                list1 = [1,2]
                head = list1.head.get
                node = [1].filter (x: x == list1.head.get)
                None
            |]
        expectedConnections list1Id nodeId headId =
            [ (outPortRef list1Id mempty, inPortRef nodeId [Arg 0])
            , (outPortRef list1Id mempty, inPortRef headId [Self, Self])
            ]
        prepare gl = do
            Just list1Id         <- findNodeIdByName gl "list1"
            Just nodeId          <- findNodeIdByName gl "node"
            Just headId          <- findNodeIdByName gl "head"
            Graph.withGraph gl . runASTOp $ (list1Id, nodeId, headId, )
                <$> GraphBuilder.buildConnections
        in testCase code code $ \gl -> do
            (list1Id, nodeId, headId, connections) <- prepare gl
            connections `shouldMatchList` expectedConnections list1Id nodeId headId
    it "shows port on List.filter with inline lambda after typechecking" $ let
        code = [r|
            def main:
                node = [].filter (x: True)
                None
            |]
        aliasPort = Port
            mempty
            "alias"
            (TCons "List" [TVar "a"])
            (WithDefault
                (Expression
                    "Std.Base.List.Empty . filter x: \171\&2\187\&Std.Base.Bool.True"
                )
            )
        selfPort  = Port
            [Self]
            "self"
            (TCons "List" [TVar "a"])
            (WithDefault (Expression "Std.Base.List.Empty"))
        argPort   = Port
            [Arg 0]
            "arg0"
            (TLam (TVar "a") (TCons "Bool" []))
            (WithDefault (Expression "x: \171\&2\187\&Std.Base.Bool.True"))
        expectedOutPorts = LabeledTree
            (InPorts
                (Just (LabeledTree def selfPort))
                Nothing
                [LabeledTree def argPort])
            aliasPort
        nodeName = "node"
        prepare gl = do
            Just node <- findNodeByName gl nodeName
            pure $ node ^. Node.inPorts
        in testCaseWithTC code code noAction $ \gl -> do
            inPorts <- prepare gl
            inPorts `shouldBe` expectedOutPorts
    it "connects to output port of toplevel function" $ let
        initialCode = [r|
            def main:
                number1 = 4
                None
            |]
        expectedCode = [r|
            def main:
                number1 = 4
                number1
            |]
        action gl = do
            Just number1       <- findNodeIdByName gl "number1"
            Just outputSidebar <- view Graph.outputSidebar <$> Graph.getGraph gl
            let outputSidebarNodeId = outputSidebar ^. Node.nodeId
            Graph.connect gl (outPortRef number1 [])
                (InPortRef' $ inPortRef outputSidebarNodeId [])
        in testCase initialCode expectedCode $ \gl -> do
            action gl
    it "connects to output port of toplevel function and undos" $ let
        initialCode = [r|
            def main:
                number1 = 4
                None
            |]
        action gl = do
            Just number1       <- findNodeIdByName gl "number1"
            Just outputSidebar <- view Graph.outputSidebar <$> Graph.getGraph gl
            let outputSidebarNodeId = outputSidebar ^. Node.nodeId

            preparedUndo       <- Graph.withGraph gl . runASTOp $
                GraphBuilder.getNodeCode outputSidebarNodeId
            Graph.connect gl (outPortRef number1 [])
                (InPortRef' $ inPortRef outputSidebarNodeId [])
            Graph.setNodeExpression gl outputSidebarNodeId preparedUndo
        in testCase initialCode initialCode $ \gl -> do
            action gl
    it "connects to output port of lambda" $ let
        initialCode = [r|
            def main:
                foo = x:
                    number1 = 4
                    x
                None
            |]
        expectedCode = [r|
            def main:
                foo = x:
                    number1 = 4
                    number1
                None
            |]
        action gl = do
            Just foo           <- findNodeIdByName gl "foo"
            let fooGL = gl |>| foo
            Just number1       <- findNodeIdByName fooGL "number1"
            Just outputSidebar <- view Graph.outputSidebar
                <$> Graph.getGraph fooGL
            let outputSidebarNodeId = outputSidebar ^. Node.nodeId

            Graph.connect fooGL (outPortRef number1 [])
                (InPortRef' $ inPortRef outputSidebarNodeId [])
        in testCase initialCode expectedCode $ \gl -> do
            action gl
    it "connects to output port of lambda and undos" $ let
        initialCode = [r|
            def main:
                foo = x:
                    number1 = 4
                    x
                None
            |]
        action gl = do
            Just foo           <- findNodeIdByName gl "foo"
            let fooGL = gl |>| foo
            Just number1       <- findNodeIdByName fooGL "number1"
            Just outputSidebar <- view Graph.outputSidebar
                <$> Graph.getGraph fooGL
            let outputSidebarNodeId = outputSidebar ^. Node.nodeId

            preparedUndo       <- Graph.withGraph fooGL . runASTOp $
                GraphBuilder.getNodeCode outputSidebarNodeId
            Graph.connect fooGL (outPortRef number1 [])
                (InPortRef' $ inPortRef outputSidebarNodeId [])
            Graph.setNodeExpression fooGL outputSidebarNodeId preparedUndo
        in testCase initialCode initialCode $ \gl -> do
            action gl