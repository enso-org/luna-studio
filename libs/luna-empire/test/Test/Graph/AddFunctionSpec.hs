module Test.Graph.AddFunctionSpec (spec) where

import Empire.Prelude

import qualified LunaStudio.Data.Node         as Node

import LunaStudio.Data.GraphLocation  ((|>|))
import Test.Hspec                     (Spec, describe, it)
import Test.Hspec.Empire              (addNode, emptyCodeTemplate,
                                       runTests, testCase)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "adding function tests" $ do
    describe "adding function inside function" $ do
        it "adds def foo inside main and validates it" $ let
            expectedCode = [r|
                import Std.Base

                def main:
                    def foo:
                        None
                    None
                |]
            in testCase emptyCodeTemplate expectedCode $ \gl ->
                addNode gl "def foo"
        it "adds def foo inside main and adds a node inside" $ let
            expectedCode = [r|
                import Std.Base

                def main:
                    def foo:
                        number1 = 4
                        None
                    None
                |]
            in testCase emptyCodeTemplate expectedCode $ \gl -> do
                node <- addNode gl "def foo"
                addNode (gl |>| node ^. Node.nodeId) "4"

        it "adds def foo inside main and adds a function inside" $ let
            expectedCode = [r|
                import Std.Base

                def main:
                    def foo:
                        def bar:
                            None
                        None
                    None
                |]
            in testCase emptyCodeTemplate expectedCode $ \gl -> do
                node <- addNode gl "def foo"
                addNode (gl |>| node ^. Node.nodeId) "def bar"
