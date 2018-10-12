module Test.Graph.NodeVisibilitySpec (spec) where

import Empire.Prelude

import qualified Empire.ASTOps.Read           as ASTRead
import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Data.Graph            as Graph
import qualified LunaStudio.Data.Graph        as Graph
import qualified LunaStudio.Data.Node         as Node
import qualified LunaStudio.Data.Port         as Port
import qualified LunaStudio.Data.PortRef      as PortRef

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.Connection     (Connection (Connection))
import LunaStudio.Data.GraphLocation  (GraphLocation (GraphLocation), top)
import LunaStudio.Data.LabeledTree    (LabeledTree (LabeledTree))
import LunaStudio.Data.Point          (Point (Point))
import LunaStudio.Data.Port           (InPortIndex (Arg), InPorts (InPorts),
                                       OutPortIndex (Projection),
                                       OutPorts (OutPorts), Port (Port),
                                       PortState (Connected, NotConnected, WithDefault))
import LunaStudio.Data.PortDefault    (PortDefault (Expression))
import LunaStudio.Data.PortRef        (AnyPortRef (InPortRef', OutPortRef'))
import LunaStudio.Data.TextDiff       (TextDiff (TextDiff))
import LunaStudio.Data.TypeRep        (TypeRep (TStar))
import Test.Hspec                     (Spec, describe, it)
import Test.Hspec.Empire              (addNode, connectToInput,
                                       emptyCodeTemplate, findNodeByName,
                                       findNodeIdByName, inPortRef, mkAliasPort,
                                       mkAllPort, mkSelfPort, outPortRef,
                                       runTests, testCase, testCaseWithMarkers,
                                       xitWithReason)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldSatisfy)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "graph building" $ do    
    describe "nodes visibility" $ do
        it "shows use of variable as node" $ let
            code = [r|
                import Std.Base

                def main:
                    foo = 1
                    foo
                    None
                |]
            prepare gl = Graph.getNodes gl
            in testCase code code $ \loc -> do
                nodes <- prepare loc
                nodes `shouldSatisfy` (== 2) . length
        it "shows use of pattern matched variable as node" $ let
            code = [r|
                import Std.Base

                def main:
                    (a, b) = (1, 2)
                    a
                    None
                |]
            prepare gl = Graph.getNodes gl
            in testCase code code $ \loc -> do
                nodes <- prepare loc
                nodes `shouldSatisfy` (== 2) . length
        it "shows use of expression as node" $ let
            code = [r|
                import Std.Base

                def main:
                    foo = 1
                    foo
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    foo = 1
                    foo.bar
                    None
                |]
            prepare gl = do
                nodes      <- Graph.getNodes gl
                parseError <- Graph.withUnit (top gl) $
                    use $ Graph.userState . Graph.clsParseError
                return (nodes, parseError)
            in testCase code expectedCode $ \loc@(GraphLocation file _) -> do
                Graph.substituteCodeFromPoints file
                    [TextDiff (Just (Point 7 4, Point 7 4)) ".bar" Nothing]
                (nodes, parseError) <- prepare loc
                nodes      `shouldSatisfy` (== 2) . length
                parseError `shouldSatisfy` isNothing
