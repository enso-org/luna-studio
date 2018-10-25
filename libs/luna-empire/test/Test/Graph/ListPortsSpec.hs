module Test.Graph.ListPortsSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified LunaStudio.Data.Node         as Node

import Control.Lens                   ((^..))
import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.GraphLocation  ((|>|))
import LunaStudio.Data.LabeledTree    (LabeledTree(..))
import LunaStudio.Data.Port           (InPorts(..), InPortIndex(..), Port(..),
                                       PortState(WithDefault))
import LunaStudio.Data.PortDefault    (PortDefault(..), PortValue(..))
import LunaStudio.Data.TypeRep        (TypeRep(TCons, TVar))
import Test.Hspec                     (Spec, describe, it)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Test.Hspec.Empire              (addNode, findNodeByName,
                                       findNodeIdByName, noAction,
                                       mkAliasPort, runTests, testCaseWithTC)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "list ports tests" $ do
    it "shows consistent names of list ports" $ let
        code = [r|
            import Std.Base

            def main:
                l = [1,2,3,4,5]
                None
            |]
        intTp = TCons "Int" []
        lit   = WithDefault . Constant . IntValue
        expectedPorts = LabeledTree
            (InPorts
                mempty
                mempty
                [
                    LabeledTree def (Port [Arg 0] "arg0" intTp (lit 1))
                  , LabeledTree def (Port [Arg 1] "arg1" intTp (lit 2))
                  , LabeledTree def (Port [Arg 2] "arg2" intTp (lit 3))
                  , LabeledTree def (Port [Arg 3] "arg3" intTp (lit 4))
                  , LabeledTree def (Port [Arg 4] "arg4" intTp (lit 5))
                ])
            (Port mempty "alias" (TCons "List" [intTp])
                (WithDefault (Expression listExpr)))
        listExpr = "a0: a1: Std.Base.List.Prepend a0 a1 1 "
                <> "(a2: a3: Std.Base.List.Prepend a2 a3 2 "
                <> "(a4: a5: Std.Base.List.Prepend a4 a5 3 "
                <> "(a6: a7: Std.Base.List.Prepend a6 a7 4 "
                <> "(a8: a9: Std.Base.List.Prepend a8 a9 5 Std.Base.List.Empty))))"
        prepare gl = do
            Just l <- findNodeByName gl "l"
            pure $ l ^. Node.inPorts
        in testCaseWithTC code code noAction $ \gl _ -> do
            ports <- prepare gl
            ports `shouldBe` expectedPorts
    it "shows names of List.filter arguments" $ let
        code = [r|
            import Std.Base

            def main:
                l = [1,2].filter even
                None
            |]
        intTp = TCons "Int" []
        lit   = WithDefault . Constant . IntValue
        expectedPorts = LabeledTree
            (InPorts
                (Just $ LabeledTree
                    (InPorts
                        mempty
                        mempty
                        [
                            LabeledTree def (Port [Self, Arg 0] "arg0" intTp (lit 1))
                          , LabeledTree def (Port [Self, Arg 1] "arg1" intTp (lit 2))
                        ])
                    (Port [Self] "self" (TCons "List" [intTp]) (WithDefault (Expression listExpr))))
                mempty
                [LabeledTree def (Port [Arg 0] "f" (TVar "a1") (WithDefault (Expression "even")))])
            (Port mempty "alias" (TCons "List" [intTp])
                (WithDefault (Expression $ listExpr <> " . filter even")))
        listExpr = "a0: a1: Std.Base.List.Prepend a0 a1 1 "
                <> "(a2: a3: Std.Base.List.Prepend a2 a3 2 Std.Base.List.Empty)"
        prepare gl resolver = do
            Just l   <- findNodeIdByName gl "l"
            tcUpdate <- Graph.withGraph gl . runASTOp $
                GraphBuilder.buildNodeTypecheckUpdate resolver l
            case tcUpdate of
                Node.ExpressionUpdate _id inPorts _ -> pure inPorts
                _                                   -> error "wrong TC update"
        in testCaseWithTC code code noAction $ \gl resolver -> do
            ports <- prepare gl resolver
            ports `shouldBe` expectedPorts
