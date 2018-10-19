module Test.Graph.ConnectionsSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.Port           (InPortIndex (Arg, Self))
import Test.Hspec                     (Spec, it)
import Test.Hspec.Empire              (findNodeIdByName, inPortRef, outPortRef,
                                       runTests, testCase)
import Test.Hspec.Expectations.Lifted (shouldMatchList)
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

