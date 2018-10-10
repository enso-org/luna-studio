module Test.Graph.PatternMatchSpec (spec) where

import Empire.Prelude

import qualified Empire.ASTOps.Read           as ASTRead
import qualified Empire.Commands.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified LunaStudio.Data.Graph        as Graph
import qualified LunaStudio.Data.Node         as Node
import qualified LunaStudio.Data.Port         as Port
import qualified LunaStudio.Data.PortRef      as PortRef

import Empire.ASTOp                   (runASTOp)
import LunaStudio.Data.GraphLocation  ((|>|))
import LunaStudio.Data.Connection     (Connection (Connection))
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
                                       mkAllPort, mkSelfPort, outPortRef,
                                       runTests, testCase, testCaseWithMarkers,
                                       xitWithReason)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "pattern match tests" $ do
    describe "nodes modifications tests" $ do
        it "adds a node pattern-matching on simple value" $ let
            expectedCode = [r|
                import Std.Base

                def main:
                    Just a = Just 1
                    None
                |]
            in testCase emptyCodeTemplate expectedCode $ \gl ->
                addNode gl "Just a = Just 1"

        it "adds a node pattern-matching on nested value" $ let
            expectedCode = [r|
                import Std.Base

                def main:
                    (Just (Just (Foo a b c)), Nothing, (Bar x y)) = foo
                    None
                |]
            in testCase emptyCodeTemplate expectedCode $ \gl ->
                addNode gl "(Just (Just (Foo a b c)), Nothing, (Bar x y)) = foo"

        it "renames a node to pattern-matching on simple value" $ let
            initialCode  = [r|
                import Std.Base

                def main:
                    a = Just 1
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    Just a = Just 1
                    None
                |]
            in testCase initialCode expectedCode $ \gl -> do
                Just nid <- findNodeIdByName gl "a"
                Graph.renameNode gl nid "Just a"

        it "adds a marker in proper place in lambda pattern-matching on an argument with simple value" $ let
            expectedCode = [r|
                import Std.Base

                «0»def main:
                    «1»foo = (Just x): y: z: «2»x + y
                    None
                |]
            in testCaseWithMarkers emptyCodeTemplate expectedCode $ \gl ->
                addNode gl "foo = (Just x): y: z: x + y"

        it "adds a marker in proper place in lambda pattern-matching on an argument with nested value" $ let
            expectedCode = [r|
                import Std.Base

                «0»def main:
                    «1»foo = (Just (Just (Foo x b c))): y: z: «2»x + y
                    None
                |]
            in testCaseWithMarkers emptyCodeTemplate expectedCode $ \gl ->
                addNode gl "foo = (Just (Just (Foo x b c))): y: z: x + y"

    describe "connections and ports tests" $ do
        it "contains proper connections and ports on a pattern matching node" $ let
            code = [r|
                import Std.Base

                def main:
                    foo = Tuple2 1 2
                    Tuple2 x y = foo
                    bar = x + 1
                    None
                |]
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected ])
                (mkAllPort "Tuple2 x y" NotConnected)
            expectedConnections fooId barId patternMatchId =
                    [ (outPortRef fooId mempty, inPortRef patternMatchId mempty)
                    , (outPortRef patternMatchId [Projection 0], inPortRef barId [Arg 0]) ]
            prepare gl = do
                Just fooId          <- findNodeIdByName gl "foo"
                Just barId          <- findNodeIdByName gl "bar"
                Just patternMatchId <- findNodeIdByName gl "Tuple2 x y"
                Graph.withGraph gl . runASTOp $ (fooId, barId, , )
                    <$> GraphBuilder.buildNode patternMatchId
                    <*> GraphBuilder.buildConnections
            in testCase code code $ \gl -> do
                (fooId, barId, patternMatch, connections) <- prepare gl
                let patternMatchId       = patternMatch ^. Node.nodeId
                    patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                connections          `shouldMatchList` expectedConnections fooId barId patternMatchId

        it "contains proper connections and ports on a pattern matching node with tuple pattern" $ let
            code = [r|
                import Std.Base

                def main:
                    foo = (1, 2)
                    (x, y) = foo
                    bar = x + 1
                    None
                |]
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected ])
                (mkAllPort "(x, y)" NotConnected)
            expectedConnections fooId barId patternMatchId =
                [ (outPortRef fooId mempty, inPortRef patternMatchId mempty)
                , (outPortRef patternMatchId [Projection 0], inPortRef barId [Arg 0]) ]
            prepare gl = do
                Just fooId          <- findNodeIdByName gl "foo"
                Just patternMatchId <- findNodeIdByName gl "(x, y)"
                Just barId          <- findNodeIdByName gl "bar"
                Graph.withGraph gl . runASTOp $ (fooId,barId, , )
                    <$> GraphBuilder.buildNode patternMatchId
                    <*> GraphBuilder.buildConnections
            in testCase code code $ \gl -> do
                (fooId, barId, patternMatch, connections) <- prepare gl
                let patternMatchId       = patternMatch ^. Node.nodeId
                    patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                connections          `shouldMatchList` expectedConnections fooId barId patternMatchId

        it "contains proper connections for nested pattern use" $ let
            code = [r|
                import Std.Base

                def main:
                    (a, b, (c, d)) = (1, 2, (3, 4))
                    c1 = c
                    None
                |]
            expectedConnections patternMatchId c1Id =
                [ (outPortRef patternMatchId [Projection 2, Projection 0], inPortRef c1Id mempty) ]
            prepare gl = do
                Just patternMatchId <- findNodeIdByName gl "(a, b, (c, d))"
                Just c1Id           <- findNodeIdByName gl "c1"
                Graph.withGraph gl . runASTOp $ (patternMatchId, c1Id, )
                    <$> GraphBuilder.buildConnections
            in testCase code code $ \gl -> do
                (patternMatchId, c1Id, connections) <- prepare gl
                connections `shouldMatchList` expectedConnections patternMatchId c1Id

        it "contains proper ports for pattern match on custom class" $ let
            code = [r|
                import Std.Base

                class Vector:
                    x y z :: Int

                def main:
                    v = Vector 1 2 3
                    Vector a b c = v
                    None
                |]
            prepare gl = do
                Just patternMatch <- findNodeByName gl "Vector a b c"
                pure patternMatch
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "a" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "b" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "c" TStar NotConnected ])
                (mkAllPort "Vector a b c" NotConnected)
            in testCase code code $ \gl -> do
                patternMatch <- prepare gl
                let patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchOutPorts `shouldBe` expectedPatternMatchOutPorts

        it "connects two outputs when one of them is pattern match" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    vector1 = Vector x y z
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = myVec1
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                (InPorts
                    (Just . LabeledTree def $ mkSelfPort NotConnected)
                    mempty
                    mempty)
                (mkAliasPort Connected)
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "z" TStar NotConnected ])
                (mkAllPort "Vector x y z" NotConnected)
            expectedConnections myVec1Id patternMatchId = [
                ( outPortRef myVec1Id mempty, inPortRef patternMatchId mempty ) ]
            prepare gl = do
                Just myVec1Id  <- findNodeIdByName gl "myVec1"
                Just vector1Id <- findNodeIdByName gl "vector1"
                pure (myVec1Id, vector1Id)
            getResult gl vector1Id = Graph.withGraph gl . runASTOp $ (,,)
                <$> GraphBuilder.buildNode vector1Id
                <*> GraphBuilder.buildConnections
                <*> (ASTRead.getASTPointer vector1Id >>= ASTRead.varIsPatternMatch)
            in testCase initialCode expectedCode $ \gl -> do
                (myVec1Id, vector1Id) <- prepare gl
                Graph.connect
                    gl
                    (outPortRef myVec1Id mempty)
                    (OutPortRef' $ outPortRef vector1Id mempty)
                (patternMatch, connections, isPatternMatch) <- getResult gl vector1Id
                let patternMatchId       = vector1Id
                    patternMatchInPorts  = patternMatch ^. Node.inPorts
                    patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchInPorts  `shouldBe`        expectedPatternMatchInPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                connections          `shouldMatchList` expectedConnections myVec1Id patternMatchId
                isPatternMatch       `shouldBe`        True

        it "disconnects pattern match" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = myVec1
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = None
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                (InPorts mempty mempty mempty)
                (mkAliasPort . WithDefault $ Expression "None")
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "z" TStar NotConnected ])
                (mkAllPort "Vector x y z" NotConnected)
            prepare gl = do
                Just patternMatchId <- findNodeIdByName gl "Vector x y z"
                pure patternMatchId
            getResult gl patternMatchId = Graph.withGraph gl . runASTOp $ (,,)
                <$> GraphBuilder.buildNode patternMatchId
                <*> GraphBuilder.buildConnections
                <*> (ASTRead.getASTPointer patternMatchId >>= ASTRead.varIsPatternMatch)
            in testCase initialCode expectedCode $ \gl -> do
                patternMatchId <- prepare gl
                Graph.disconnect gl $ inPortRef patternMatchId mempty
                (patternMatch, connections, isPatternMatch) <- getResult gl patternMatchId
                let patternMatchOutPorts = patternMatch ^. Node.outPorts
                    patternMatchInPorts  = patternMatch ^. Node.inPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                patternMatchInPorts  `shouldBe`        expectedPatternMatchInPorts
                connections          `shouldMatchList` mempty
                isPatternMatch       `shouldBe`        True

        it "connects to pattern match, disconnects and connects again" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    vector1 = Vector x y z
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = myVec1
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                (InPorts
                    (Just . LabeledTree def $ mkSelfPort NotConnected)
                    mempty
                    mempty )
                (mkAliasPort Connected)
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "z" TStar NotConnected ])
                (mkAllPort "Vector x y z" NotConnected)
            expectedConnections myVec1Id patternMatchId =
                [ (outPortRef myVec1Id mempty, inPortRef patternMatchId mempty) ]
            prepare gl = do
                Just myVec1Id       <- findNodeIdByName gl "myVec1"
                Just patternMatchId <- findNodeIdByName gl "vector1"
                pure (myVec1Id, patternMatchId)
            getResult gl patternMatchId = Graph.withGraph gl . runASTOp $ (,,)
                <$> GraphBuilder.buildNode patternMatchId
                <*> GraphBuilder.buildConnections
                <*> (ASTRead.getASTPointer patternMatchId >>= ASTRead.varIsPatternMatch)
            in testCase initialCode expectedCode $ \gl -> do
                (myVec1Id, patternMatchId) <- prepare gl
                Graph.connect
                    gl
                    (outPortRef myVec1Id mempty)
                    (OutPortRef' $ outPortRef patternMatchId mempty)
                Graph.disconnect gl $ inPortRef patternMatchId mempty
                Graph.connect gl
                    (outPortRef myVec1Id mempty)
                    (InPortRef' $ inPortRef patternMatchId mempty)
                (patternMatch, connections, isPatternMatch) <- getResult gl patternMatchId
                let patternMatchInPorts  = patternMatch ^. Node.inPorts
                    patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchInPorts  `shouldBe`        expectedPatternMatchInPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                connections          `shouldMatchList` expectedConnections myVec1Id patternMatchId
                isPatternMatch       `shouldBe`        True

        it "connects deconstructed value to other nodes" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = myVec1
                    lambda1 = a: b: a + b
                    function1 = lambda1
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    Vector x y z = myVec1
                    lambda1 = a: b: a + b
                    function1 = lambda1 x y
                    None
                |]
            expectedFunction1InPorts = LabeledTree
                (InPorts
                    (Just . LabeledTree def $ mkSelfPort NotConnected)
                    mempty
                    [ LabeledTree def $ Port [Arg 0] "x" TStar Connected
                    , LabeledTree def $ Port [Arg 1] "y" TStar Connected ])
                (mkAliasPort . WithDefault $ Expression "lambda1 x y")
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "x" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "y" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "z" TStar NotConnected ])
                (mkAllPort "Vector x y z" NotConnected)
            expectedConnections patternMatchId function1Id =
                [ (outPortRef patternMatchId [Projection 0], inPortRef function1Id [Arg 0])
                , (outPortRef patternMatchId [Projection 1], inPortRef function1Id [Arg 1]) ]
            prepare gl = do
                Just patternMatchId <- findNodeIdByName gl "Vector x y z"
                Just function1Id    <- findNodeIdByName gl "function1"
                pure (patternMatchId, function1Id)
            getResult gl patternMatchId function1Id = do
                let isRelevantConnection (outRef, inRef)
                        = outRef ^. PortRef.srcNodeId == patternMatchId
                        && inRef ^. PortRef.dstNodeId == function1Id
                    filterConnections = filter isRelevantConnection
                Graph.withGraph gl . runASTOp $ (,,)
                    <$> GraphBuilder.buildNode patternMatchId
                    <*> GraphBuilder.buildNode function1Id
                    <*> (filterConnections <$> GraphBuilder.buildConnections)
            in testCase initialCode expectedCode $ \gl -> do
                (patternMatchId, function1Id) <- prepare gl
                connectToInput
                    gl
                    (outPortRef patternMatchId [Projection 0])
                    (inPortRef function1Id [Arg 0])
                connectToInput
                    gl
                    (outPortRef patternMatchId [Projection 1])
                    (inPortRef function1Id [Arg 1])
                (patternMatch, function1, connections) <- getResult gl patternMatchId function1Id
                let patternMatchOutPorts = patternMatch ^. Node.outPorts
                    function1InPorts     = function1    ^. Node.inPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                function1InPorts     `shouldBe`        expectedFunction1InPorts
                connections          `shouldMatchList` expectedConnections patternMatchId function1Id

        it "connects two outputs when one of them is nested pattern match with literals" $ let
            initialCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    someCons1 = SomeCons (Just a) 0 "foo" x
                    None
                |]
            expectedCode = [r|
                import Std.Base

                def main:
                    myVec1 = myVec
                    SomeCons (Just a) 0 "foo" x = myVec1
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                ( InPorts
                    (Just $ LabeledTree def $ mkSelfPort NotConnected)
                    mempty
                    mempty )
                (mkAliasPort Connected)
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts [
                    LabeledTree
                        (OutPorts [ LabeledTree def
                            (Port [Projection 0, Port.Projection 0] "a" TStar NotConnected) ])
                        (Port [Projection 0] "Just a" TStar NotConnected)
                    , LabeledTree def $ Port [Projection 1] "0"       TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "\"foo\"" TStar NotConnected
                    , LabeledTree def $ Port [Projection 3] "x"       TStar NotConnected
                    ])
                (mkAllPort "SomeCons (Just a) 0 \"foo\" x" NotConnected)
            expectedConnections myVec1Id patternMatchId =
                [ (outPortRef myVec1Id mempty, inPortRef patternMatchId mempty) ]
            prepare gl = do
                Just myVec1Id    <- findNodeIdByName gl "myVec1"
                Just someCons1Id <- findNodeIdByName gl "someCons1"
                pure (myVec1Id, someCons1Id)
            getResult gl patternMatchId = Graph.withGraph gl . runASTOp $ (,,)
                <$> GraphBuilder.buildNode patternMatchId
                <*> GraphBuilder.buildConnections
                <*> (ASTRead.getASTPointer patternMatchId >>= ASTRead.varIsPatternMatch)
            in testCase initialCode expectedCode $ \gl -> do
                (myVec1Id, someCons1Id) <- prepare gl
                Graph.connect
                    gl
                    (outPortRef myVec1Id mempty)
                    (OutPortRef' $ outPortRef someCons1Id mempty)
                (patternMatch, connections, isPatternMatch) <- getResult gl someCons1Id
                let patternMatchId       = someCons1Id
                    patternMatchOutPorts = patternMatch ^. Node.outPorts
                    patternMatchInPorts  = patternMatch ^. Node.inPorts
                patternMatchInPorts  `shouldBe`        expectedPatternMatchInPorts
                patternMatchOutPorts `shouldBe`        expectedPatternMatchOutPorts
                connections          `shouldMatchList` expectedConnections myVec1Id patternMatchId
                isPatternMatch       `shouldBe`        True

    describe "lambdas pattern matching on their arguments" $ do
        xitWithReason "supports lambdas pattern matching on their argument" "waiting for new printer" $ let
            code = [r|
                import Std.Base

                def main:
                    lambda1 = (Foobar a b c): b
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                (InPorts
                    mempty
                    mempty
                    [ LabeledTree def $ Port [Arg 0] "arg0" TStar NotConnected ])
                (mkAliasPort . WithDefault $ Expression "(Foobar a b c): b")
            expectedInputOutPorts = [ LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0, Projection 0] "a" TStar NotConnected
                    , LabeledTree def $ Port [Projection 0, Projection 1] "b" TStar NotConnected
                    , LabeledTree def $ Port [Projection 0, Projection 2] "c" TStar NotConnected ])
                (Port [Projection 0] "Foobar a b c" TStar NotConnected) ]
            expectedOutputInPorts = LabeledTree
                (InPorts mempty mempty mempty)
                (Port mempty "output" TStar Connected)
            expectedConnections inputId outputId = [ Connection
                (outPortRef inputId  [Projection 0, Projection 1])
                (inPortRef  outputId mempty) ]
            prepare gl = do
                Just lambda1 <- findNodeByName gl "lambda1"
                let lambda1Gl = gl |>| (lambda1 ^. Node.nodeId)
                Graph.withGraph lambda1Gl . runASTOp $ do
                    graph           <- GraphBuilder.buildGraph
                    (input, output) <- GraphBuilder.buildEdgeNodes
                    pure
                        ( lambda1
                        , graph ^. Graph.nodes
                        , graph ^. Graph.connections
                        , input
                        , output)
            in testCase code code $ \gl -> do
                (patternMatch, nodes, connections, input, output) <- prepare gl
                let inputId             = input        ^. Node.nodeId
                    outputId            = output       ^. Node.nodeId
                    patternMatchInPorts = patternMatch ^. Node.inPorts
                    inputOutPorts       = input        ^. Node.inputEdgePorts
                    outputInPorts       = output       ^. Node.outputEdgePorts
                patternMatchInPorts `shouldBe`        expectedPatternMatchInPorts
                inputOutPorts       `shouldBe`        expectedInputOutPorts
                outputInPorts       `shouldBe`        expectedOutputInPorts
                nodes               `shouldBe`        mempty
                connections         `shouldMatchList` expectedConnections inputId outputId

        xitWithReason "supports multi-parameter lambdas pattern matching on their arguments" "waiting for new printer" $ let
            code = [r|
                import Std.Base

                def main:
                    lambda1 = (Foobar a b (Just c)): x: (Quux y z): c
                    None
                |]
            expectedPatternMatchInPorts = LabeledTree
                (InPorts
                    mempty
                    mempty
                    [ LabeledTree def $ Port [Arg 0] "arg0" TStar NotConnected
                    , LabeledTree def $ Port [Arg 1] "x"    TStar NotConnected
                    , LabeledTree def $ Port [Arg 2] "arg2" TStar NotConnected ])
                (mkAliasPort . WithDefault $ Expression "(Foobar a b (Just c)): x: (Quux y z): c")
            expectedInputOutPorts =
                [ LabeledTree
                    (OutPorts
                        [ LabeledTree def $ Port [Projection 0, Projection 0] "a" TStar NotConnected
                        , LabeledTree def $ Port [Projection 0, Projection 1] "b" TStar NotConnected
                        , LabeledTree
                            (OutPorts
                                [ LabeledTree def $ Port [Projection 0, Projection 2, Projection 0] "c" TStar NotConnected ])
                            (Port [Projection 0, Projection 2] "Just c" TStar NotConnected) ])
                    (Port [Projection 0] "Foobar a b (Just c)" TStar NotConnected)
                , LabeledTree def $ Port [Projection 1] "x" TStar NotConnected
                , LabeledTree
                    (OutPorts
                        [ LabeledTree def $ Port [Projection 2, Projection 0] "y" TStar NotConnected
                        , LabeledTree def $ Port [Projection 2, Projection 1] "z" TStar NotConnected ])
                    (Port [Projection 2] "Quux y z" TStar NotConnected) ]
            expectedOutputInPorts = LabeledTree
                (InPorts mempty mempty mempty)
                (Port mempty "output" TStar Connected)
            expectedConnections inputId outputId = [ Connection
                (outPortRef inputId  [Projection 0, Projection 2, Projection 0])
                (inPortRef  outputId mempty) ]
            prepare gl = do
                Just lambda1 <- findNodeByName gl "lambda1"
                let lambda1Gl = gl |>| (lambda1 ^. Node.nodeId)
                Graph.withGraph lambda1Gl . runASTOp $ do
                    graph           <- GraphBuilder.buildGraph
                    (input, output) <- GraphBuilder.buildEdgeNodes
                    pure
                        ( lambda1
                        , graph ^. Graph.nodes
                        , graph ^. Graph.connections
                        , input
                        , output)
            in testCase code code $ \gl -> do
                (patternMatch, nodes, connections, input, output) <- prepare gl
                let inputId             = input        ^. Node.nodeId
                    outputId            = output       ^. Node.nodeId
                    patternMatchInPorts = patternMatch ^. Node.inPorts
                    inputOutPorts       = input        ^. Node.inputEdgePorts
                    outputInPorts       = output       ^. Node.outputEdgePorts
                patternMatchInPorts `shouldBe`        expectedPatternMatchInPorts
                inputOutPorts       `shouldBe`        expectedInputOutPorts
                outputInPorts       `shouldBe`        expectedOutputInPorts
                nodes               `shouldBe`        mempty
                connections         `shouldMatchList` expectedConnections inputId outputId
