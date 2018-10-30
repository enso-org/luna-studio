module Test.Graph.ConnectionsSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified LunaStudio.Data.Node         as Node

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.Port           (InPortIndex (Arg, Self), InPorts (InPorts),
                                       LabeledTree (LabeledTree), Port (Port),
                                       PortState (WithDefault))
import LunaStudio.Data.TypeRep        (TypeRep (TCons, TLam, TVar))
import LunaStudio.Data.PortDefault    (PortDefault (Expression))
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
        in testCaseWithTC code code noAction $ \gl _ -> do
            inPorts <- prepare gl
            inPorts `shouldBe` expectedOutPorts