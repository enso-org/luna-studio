module Test.Graph.EnterNodeSpec (spec) where

import Empire.Prelude

import qualified Empire.ASTOps.Read           as ASTRead
import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified LunaStudio.Data.Breadcrumb   as Breadcrumb
import qualified LunaStudio.Data.Graph        as Graph
import qualified LunaStudio.Data.Node         as Node
import qualified LunaStudio.Data.Port         as Port
import qualified LunaStudio.Data.PortRef      as PortRef

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.Connection     (Connection (Connection))
import LunaStudio.Data.GraphLocation  (GraphLocation (GraphLocation), (|>))
import LunaStudio.Data.LabeledTree    (LabeledTree (LabeledTree))
import LunaStudio.Data.Port           (InPortIndex (Arg), InPorts (InPorts),
                                       OutPortIndex (Projection),
                                       OutPorts (OutPorts), Port (Port),
                                       PortState (Connected, NotConnected, WithDefault))
import LunaStudio.Data.PortDefault    (PortDefault (Expression))
import LunaStudio.Data.PortRef        (AnyPortRef (InPortRef', OutPortRef'))
import LunaStudio.Data.TypeRep        (TypeRep (TStar))
import Test.Hspec                     (Spec, describe, it)
import Test.Hspec.Empire              (addNode, connectToInput,
                                       emptyCodeTemplate, findNodeByName,
                                       findNodeIdByName, inPortRef, mkAliasPort,
                                       mkAllPort, mkSelfPort, noAction,
                                       outPortRef, runTests, testCase,
                                       testCaseWithTC, xitWithReason)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldSatisfy)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "entering nodes at use-site" $ do
    it "enters defined function in the same file" $ let
        initialCode = [r|
            def foo a:
                a

            def main:
                bar = foo 1
                None
            |]
        in testCaseWithTC initialCode initialCode noAction $ \gl _ -> do
            Just bar <- findNodeIdByName gl "bar"
            let GraphLocation file (coerce -> bc) = gl
                barGL = GraphLocation file $ coerce $ bc
                    <> [Breadcrumb.Redirection bar (Breadcrumb.Function "TestProject.Main" "foo")]
            graph <- Graph.getGraph barGL
            graph ^. Graph.connections `shouldSatisfy` (not . null)

    it "enters function from stdlib" $ let
        initialCode = [r|
            import Std.Base

            def main:
                bar = id 1
                None
            |]
        in testCaseWithTC initialCode initialCode noAction $ \gl _ -> do
            Just bar <- findNodeIdByName gl "bar"
            let GraphLocation file (coerce -> bc) = gl
                barGL = GraphLocation file $ coerce $ bc
                    <> [Breadcrumb.Redirection bar (Breadcrumb.Function "Std.Base" "id")]
            graph <- Graph.getGraph barGL
            graph ^. Graph.connections `shouldSatisfy` (not . null)

    it "enters method from stdlib" $ let
        initialCode = [r|
            import Std.Base

            def main:
                bar = 1.negate
                None
            |]
        in testCaseWithTC initialCode initialCode noAction $ \gl _ -> do
            Just bar <- findNodeIdByName gl "bar"
            let GraphLocation file (coerce -> bc) = gl
                barGL = GraphLocation file $ coerce $ bc
                    <> [Breadcrumb.Redirection bar (Breadcrumb.Method "Std.Base" "Int" "negate")]
            graph <- Graph.getGraph barGL
            graph ^. Graph.connections `shouldSatisfy` (not . null)

