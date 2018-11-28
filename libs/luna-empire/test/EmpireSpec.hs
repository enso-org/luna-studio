{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module EmpireSpec (spec) where

import           Control.Lens                    ((^..), (^?!))
import qualified Data.Graph.Store                as Store
import           Data.List                       (find)
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.ASTOps.Deconstruct       as ASTDeconstruct
import           Empire.ASTOps.Modify            (CannotRemovePortException)
import qualified Empire.ASTOps.Parse             as Parser
import           Empire.ASTOps.Print             (printExpression)
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST (isTrivialLambda)
import qualified Empire.Commands.Graph           as Graph (addNode, addPort, addPortWithConnections,
                                                           connect, disconnect,
                                                           getConnections,
                                                           getGraph,
                                                           getNodeIdForMarker,
                                                           getNodes, loadCode,
                                                           movePort,
                                                           removeNodes,
                                                           removePort,
                                                           renameNode,
                                                           renamePort,
                                                           setNodeExpression,
                                                           setNodeMeta,
                                                           withGraph, withUnit)
import qualified Empire.Commands.GraphBuilder    as GraphBuilder
import           Empire.Commands.Library         (createLibrary)
import qualified Empire.Commands.Typecheck       as Typecheck (run)
import qualified Empire.Data.AST                 as AST
import           Empire.Data.BreadcrumbHierarchy (BreadcrumbDoesNotExistException)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (breadcrumbHierarchy,
                                                  userState)
import qualified Empire.Data.Graph               as Graph hiding (Graph)
import           Empire.Empire                   (InterpreterEnv (..))
import qualified Luna.Package.Structure.Generate as Package
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (Definition))
import           LunaStudio.Data.Connection      (Connection (Connection))
import qualified LunaStudio.Data.Graph           as Graph
import           LunaStudio.Data.GraphLocation   (GraphLocation (..), (|>|))
import           LunaStudio.Data.LabeledTree     (LabeledTree (..))
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeMeta        (NodeMeta (..), position)
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortDefault     (PortDefault (Expression))
import           LunaStudio.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))
import           Empire.Prelude                  hiding (toList, seq)
import           Text.RawString.QQ               (r)
import           System.FilePath                 ((</>))
import qualified System.IO.Temp                  as Temp

import           Test.Hspec                      (Selector, Spec, around,
                                                  describe, it, parallel,
                                                  shouldBe, shouldContain,
                                                  shouldMatchList,
                                                  shouldSatisfy, shouldThrow,
                                                  xit)

import EmpireUtils


spec :: Spec
spec = around withChannels $ parallel $ do
    -- describe "imports" $ do
    --     it "filters private methods" $ \_ -> do
    --         let wd = Function.WithDocumentation Nothing
    --         let imports = Module.Imports (Map.fromList [("Klass1", wd $ Class.Class def (Map.fromList [("someMethod", undefined), ("_privateMethod", undefined)]))])
    --                                      (Map.fromList [("function", wd undefined), ("_privateFunction", wd undefined)])
    --             hints = Graph.importsToHints imports
    --         hints ^? NodeSearcher.classes . ix "Klass1" . NodeSearcher.methods . to Map.fromList . ix "_privateMethod" `shouldBe` Nothing
    --         hints ^? NodeSearcher.classes . ix "Klass1" . NodeSearcher.methods . to Map.fromList . ix "someMethod" `shouldNotBe` Nothing
    --         hints ^? NodeSearcher.functions . to Map.fromList . ix "_privateFunction" `shouldBe` Nothing
    --         hints ^? NodeSearcher.functions . to Map.fromList . ix "function" `shouldNotBe` Nothing
    --     it "filters private native functions" $ \_ -> do
    --         let wd = Function.WithDocumentation Nothing
    --         let imports = Module.Imports def
    --                                      (Map.fromList [("primFunction", wd undefined), ("#uminus#", wd undefined), ("function", wd undefined)])
    --             hints = Graph.importsToHints $ Graph.filterPrimMethods imports
    --         hints ^? NodeSearcher.functions . to Map.fromList . ix "primFunction" `shouldBe` Nothing
    --         hints ^? NodeSearcher.functions . to Map.fromList . ix "#uminus#" `shouldBe` Nothing
    --         hints ^? NodeSearcher.functions . to Map.fromList . ix "function" `shouldNotBe` Nothing
    describe "luna-empire" $ do
        it "descends into `foo = a: a` and asserts two edges inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                topLevel <- graphIDs top
                n1Level  <- Graph.getGraph (top |>| u1)
                return (topLevel, n1Level)
            withResult res $ \(topLevel, Graph.Graph n1LevelNodes _ i o _ _) -> do
                length topLevel `shouldBe` 1
                topLevel `shouldContain` [u1]
                i            `shouldSatisfy` isJust
                o            `shouldSatisfy` isJust
                n1LevelNodes `shouldSatisfy` null
        it "descends into `foo = (a: a)` and asserts two edges inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = (a: a)" def
                topLevel <- graphIDs top
                n1Level <- Graph.getGraph (top |>| u1)
                return (topLevel, n1Level)
            withResult res $ \(topLevel, Graph.Graph n1LevelNodes _ i o _ _) -> do
                length topLevel `shouldBe` 1
                topLevel `shouldContain` [u1]
                i            `shouldSatisfy` isJust
                i ^? _Just . Node.isDef `shouldBe` Just False
                o            `shouldSatisfy` isJust
                n1LevelNodes `shouldSatisfy` null
        it "asserts things about `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
            withResult res $ \node -> do
                node ^. Node.name       `shouldBe` Just "foo"
                node ^. Node.code       `shouldBe` "a: a"
                node ^. Node.expression `shouldBe` "Ⓕ"
                node ^. Node.canEnter   `shouldBe` True
        it "returns connections for deeply nested uses of node" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = 1" def
                Graph.addNode top u2 "7 + foo * 5" $ def & position . Position.x .~ 20.0
                Graph.getConnections top
            res `shouldMatchList` [
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Arg 1, Port.Arg 0])
                ]
        it "dumps proper ports for self" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = 1" def
                Graph.addNode top u2 "bar = foo . baz" $ def & position . Position.x .~ 20.0
                Graph.getNodes top
            withResult res $ \nodes -> do
                let Just bar = find (\a -> view Node.nodeId a == u2) nodes
                (bar ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []                     "alias" TStar (Port.WithDefault $ Expression "foo . baz")
                    , Port.Port [Port.Self]            "self"  TStar Port.Connected
                    , Port.Port [Port.Self, Port.Self] "self"  TStar Port.NotConnected
                    ]
        it "returns connections for deeply nested uses of node in self position" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = 1" def
                Graph.addNode top u2 "foo . prepend foo . prepend foo" $ def & position . Position.x .~ 20.0
                Graph.getConnections top
            res `shouldMatchList` [
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Arg 0]),
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Self, Port.Arg 0]),
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Self, Port.Self])
                ]
        it "returns connections for deeply nested uses of node in head position" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = 1" def
                Graph.addNode top u2 "foo foo . bar foo" $ def & position . Position.x .~ 20.0
                Graph.getConnections top
            res `shouldMatchList` [
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Arg 0]),
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Self, Port.Head]),
                    Connection (outPortRef u1 []) (inPortRef u2 [Port.Self, Port.Arg 0])
                ]
        it "makes connection to output edge" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                Graph.addNode loc' u2 "4" def
                (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (outPortRef u2 [], inPortRef out [])
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (uncurry Connection referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 1) . length)
                unsafeHead connections `shouldBe` conn
        it "connects input with output edge" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.disconnect loc' (inPortRef output [])
                let referenceConnection = (outPortRef input [Port.Projection 0], inPortRef output [])
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (uncurry Connection referenceConnection, connections)
            withResult res $ \(ref, connections) -> do
                connections `shouldMatchList` [ref]
        it "connects input edge to succ (Self)" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                Graph.addNode loc' u2 "succ" def
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (outPortRef input [Port.Projection 0], inPortRef u2 [Port.Self])
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (uncurry Connection referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "connects input edge to dummy node (Arg 0)" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "succ" def
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (outPortRef input [Port.Projection 0], inPortRef u2 [Port.Arg 0])
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (uncurry Connection referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "has proper connection inside `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "foo = a: a" def
                conns <- Graph.getConnections loc'
                edges <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                return (conns, edges)
            withResult res $ \(connections, (inputEdge, outputEdge)) -> do
                connections `shouldMatchList` [Connection (outPortRef inputEdge [Port.Projection 0]) (inPortRef outputEdge [])]
        it "shows connection inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "4" def
                Graph.addNode loc' u3 "succ" def
                let referenceConnection = (outPortRef u2 [], inPortRef u3 [Port.Self])
                uncurry (connectToInput loc') referenceConnection
                connections <- Graph.getConnections loc'
                return (uncurry Connection referenceConnection, connections)
            withResult res $ \(ref, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [ref]
        it "creates two nested lambdas and a node inside" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode (top |>| u1) u2 "bar = c: c" def
                let loc = top |>| u1 |>| u2
                Graph.addNode loc u3 "4" def
                graphIDs loc
            withResult res $ \ids -> do
                u3 `shouldSatisfy` (`elem` ids)
                u1 `shouldSatisfy` (`notElem` ids)
                u2 `shouldSatisfy` (`notElem` ids)
        it "cannot enter lambda applied to value" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode top u2 "4" def
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 0])
                Graph.getGraph top
            withResult res $ \g -> do
                let Just lambdaNode = find ((== u1) . view Node.nodeId) $ Graph._nodes g
                lambdaNode ^. Node.canEnter `shouldBe` False
        it "has no null node inside `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.getNodes (top |>| u1)
            withResult res $ \ids -> do
                ids `shouldSatisfy` null
        it "int literal has no nodes inside" $ \env -> do
            u1 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "4" def
                    graphIDs $ top |>| u1
            let breadcrumbException :: Selector BreadcrumbDoesNotExistException
                breadcrumbException = const True
            res `shouldThrow` breadcrumbException
        {-xit "properly typechecks input nodes" $ \env -> do-}
            {-u1 <- mkUUID-}
            {-(res, st) <- runEmp env $ do-}
                {-Graph.addNode top u1 "a: b: a + b" def-}
                {-let GraphLocation file _ = top-}
                {-withLibrary file (use Library.body)-}
            {-withResult res $ \g -> do-}
                {-(_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $-}
                    {-Typecheck.run emptyGraphLocation-}
                {-(res'', _) <- runEmp' env st g' $ do-}
                    {-Graph.getNodes $ top |>| u1-}
                {-withResult res'' $ \nodes' -> do-}
                    {-let Just input = find ((== "inputEdge") . view Node.name) nodes'-}
                        {-ports' = toList $ input ^. Node.ports-}
                        {-types = map (view Port.valueType) ports'-}
                    {-types `shouldMatchList` [TCons "Int" [], TCons "Int" []]-}
        {-xit "properly typechecks output nodes" $ \env -> do-}
            {-u1 <- mkUUID-}
            {-(res, st) <- runEmp env $ do-}
                {-Graph.addNode top u1 "a: b: a + b" def-}
                {-let GraphLocation file _ = top-}
                {-withLibrary file (use Library.body)-}
            {-withResult res $ \g -> do-}
                {-(_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $-}
                    {-Typecheck.run emptyGraphLocation-}
                {-(res'',_) <- runEmp' env st g' $ do-}
                    {-Graph.getNodes $ top |>| u1-}
                {-withResult res'' $ \nodes' -> do-}
                    {-let Just output' = find ((== "outputEdge") . view Node.name) nodes'-}
                        {-ports' = toList $ output' ^. Node.ports-}
                        {-types = map (view Port.valueType) ports'-}
                    {-types `shouldBe` [TCons "Int" []]-}
        {-xit "properly typechecks edges inside mock id" $ \env -> do-}
            {-u1 <- mkUUID-}
            {-(res, st) <- runEmp env $ do-}
                {-Graph.addNode top u1 "__intId" def-}
                {-let GraphLocation file _ = top-}
                {-withLibrary file (use Library.body)-}
            {-withResult res $ \g -> do-}
                {-(_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $-}
                    {-Typecheck.run emptyGraphLocation-}
                {-(res'',_) <- runEmp' env st g' $ do-}
                    {-Graph.getNodes $ top |>| u1-}
                {-withResult res'' $ \nodes' -> do-}
                    {-let Just input' = find ((== "inputEdge") . view Node.name) nodes'-}
                        {-inPorts' = toList $ input' ^. Node.ports-}
                        {-inputType = map (view Port.valueType) inPorts'-}
                    {-let Just output' = find ((== "outputEdge") . view Node.name) nodes'-}
                        {-outPorts' = toList $ output' ^. Node.ports-}
                        {-outputType = map (view Port.valueType) outPorts'-}
                    {-inputType  `shouldBe` [TCons "Int" []]-}
                    {-outputType `shouldBe` [TCons "Int" []]-}
        xit "properly typechecks second id in `mock id -> mock id`" $ \env -> do
            Temp.withSystemTempDirectory "luna-empirespec" $ \path -> do
                u1 <- mkUUID
                u2 <- mkUUID
                (res, st) <- runEmp env $ do
                    Right pkgPath <- Package.genPackageStructure (path </> "MockId") Nothing def
                    let mainLuna = pkgPath </> "src" </> "Main.luna"
                    createLibrary Nothing mainLuna
                    let loc = GraphLocation mainLuna $ Breadcrumb []
                    let initialCode = [r|
                            import Std.Base

                            def main:
                                None
                            |]
                    Graph.loadCode loc $ normalizeLunaCode initialCode
                    [main] <- filter (\n -> n ^. Node.name == Just "main") <$> Graph.getNodes loc
                    let loc' = GraphLocation mainLuna $ Breadcrumb [Definition (main ^. Node.nodeId)]
                    Graph.addNode loc' u1 "id" def
                    Graph.addNode loc' u2 "id" def
                    connectToInput loc' (outPortRef u1 []) (inPortRef u2 [Port.Arg 0])
                    Graph.withUnit (GraphLocation mainLuna def) $ do
                        g <- use userState
                        let root = g ^. Graph.clsClass
                        rooted <- runASTOp $ Store.serializeWithRedirectMap root
                        return (loc', g, rooted)
                withResult res $ \(loc, g, rooted) -> do
                    pmState <- Graph.defaultPMState
                    let interpreterEnv = InterpreterEnv (return ()) g [] def def def def
                    (_, (extractGraph -> g')) <- runEmpire env (Graph.CommandState pmState interpreterEnv) $
                        Typecheck.run loc g rooted False False
                    _ <- runEmp' env st g' $ do
                        Graph.withGraph loc $ runASTOp $ (,) <$> GraphBuilder.buildNode u1 <*> GraphBuilder.buildNode u2
                    -- withResult res'' $ \(n1, n2) -> do
                    --     view Node.inPorts n2 `shouldMatchList` [
                    --           Port.Port [Port.Arg 0] "in" (TLam (TVar "a") (TVar "a")) Port.Connected
                    --         ]
                    --     view Node.outPorts n1 `shouldMatchList` [
                    --           Port.Port [] "Output" (TLam (TVar "a") (TVar "a")) (Port.WithDefault (Expression "in: in"))
                    --         ]
                    return ()
        it "puts + inside plus lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "a: b: a + b" def
                Graph.getNodes loc'
            withResult res $ \nodes -> do
                nodes `shouldSatisfy` ((== 1) . length)
                unsafeHead nodes `shouldSatisfy` (\a -> a ^. Node.expression == "• + •")
        it "places connections between + node and output" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "a: b: a + b" def
                Graph.getConnections loc'
            withResult res $ \conns -> do
                -- one from a to +, one from b to + and one from + to output edge
                conns `shouldSatisfy` ((== 3) . length)
        it "shows connection between node and list containing it" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "[number1]" def
                Graph.getConnections top
            withResult res $ \conns -> do
                conns `shouldSatisfy` ((== 1) . length)
        it "shows connection between nodes and list containing them" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            u6 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "2" def
                Graph.addNode top u3 "2" def
                Graph.addNode top u4 "2" def
                Graph.addNode top u6 "    [   number1  , number2   ,   number3 ,number4   ]" def
                Graph.getConnections top
            withResult res $ \conns -> do
                conns `shouldSatisfy` ((== 4) . length)
        it "connects elements to a list" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "[]" def
                Graph.addNode top u2 "2" def
                Graph.addNode top u3 "3" def
                Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg 0])
                Graph.connect top (outPortRef u3 []) (InPortRef' $ inPortRef u1 [Port.Arg 1])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u1
            withResult res $ \(node) -> do
                node ^. Node.expression `shouldBe` "[\8226, \8226]"
                node ^. Node.code `shouldBe` "[number1, number2]"
        it "connects elements to a list and removes all but one" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "[]" def
                Graph.addNode top u2 "2" def
                Graph.addNode top u3 "3" def
                Graph.addNode top u4 "4" def
                Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg 0])
                Graph.connect top (outPortRef u3 []) (InPortRef' $ inPortRef u1 [Port.Arg 1])
                Graph.connect top (outPortRef u4 []) (InPortRef' $ inPortRef u1 [Port.Arg 2])
                Graph.disconnect top (inPortRef u1 [Port.Arg 0])
                Graph.disconnect top (inPortRef u1 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u1
            withResult res $ \(node) -> do
                node ^. Node.expression `shouldBe` "[\8226]"
                node ^. Node.code `shouldBe` "[number3]"
        it "replaces element in a list" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u2 "2" def
                Graph.addNode top u3 "3" def
                Graph.addNode top u1 "[    number1   ,     number2   ]" def
                Graph.connect top (outPortRef u3 []) (InPortRef' $ inPortRef u1 [Port.Arg 0])
                Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u1
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226, \8226]"
                node ^. Node.code `shouldBe` "[    number1   ,     number2   ]"
        it "replaces newly added element in a list" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u2 "2" def
                Graph.addNode top u3 "3" def
                Graph.addNode top u4 "4" def
                Graph.addNode top u1 "[    number1   ,     number2   ]" def
                Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg 2])
                Graph.connect top (outPortRef u4 []) (InPortRef' $ inPortRef u1 [Port.Arg 2])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u1
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226, \8226, \8226]"
                node ^. Node.code `shouldBe` "[    number1   ,     number2, number3   ]"
        it "disconnects the only list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "[number1]" def
                Graph.disconnect top (inPortRef u2 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u2
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[]"
                node ^. Node.code `shouldBe` "[]"
        it "disconnects the first list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "[ number1,  number2   ]" def
                Graph.disconnect top (inPortRef u3 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u3
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226]"
                node ^. Node.code `shouldBe` "[ number2   ]"
        it "disconnects the last list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "[   number1  ,  number2  ]" def
                Graph.disconnect top (inPortRef u3 [Port.Arg 1])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u3
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226]"
                node ^. Node.code `shouldBe` "[   number1  ]"
        it "disconnects all list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "[   number1  ,  number2  ]" def
                Graph.disconnect top (inPortRef u3 [Port.Arg 0])
                Graph.disconnect top (inPortRef u3 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u3
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[]"
                node ^. Node.code `shouldBe` "[]"
        it "disconnects middle list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "4" def
                Graph.addNode top u4 "[   number1  ,  number2  ,     number3]" def
                Graph.disconnect top (inPortRef u4 [Port.Arg 1])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u4
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226, \8226]"
                node ^. Node.code `shouldBe` "[   number1  ,  number3]"
        it "disconnects middle list elements" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            u5 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "4" def
                Graph.addNode top u4 "4" def
                Graph.addNode top u5 "[   number1 ,    number2  ,number3    ,    number4    ]" def
                Graph.disconnect top (inPortRef u5 [Port.Arg 2])
                Graph.disconnect top (inPortRef u5 [Port.Arg 1])
                Graph.disconnect top (inPortRef u5 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u5
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226]"
                node ^. Node.code `shouldBe` "[   number4    ]"
        it "disconnects two list elements from the end" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "2" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "4" def
                Graph.addNode top u4 "[   number1  ,  number2  ,     number3]" def
                Graph.disconnect top (inPortRef u4 [Port.Arg 2])
                Graph.disconnect top (inPortRef u4 [Port.Arg 1])
                Graph.withGraph top $ runASTOp $ do
                    GraphBuilder.buildNode u4
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "[\8226]"
                node ^. Node.code `shouldBe` "[   number1]"
        it "disallows connecting past last + 1 list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "[]" def
                    Graph.addNode top u2 "2" def
                    Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg 1])
            let parserException :: Selector AST.ConnectionException
                parserException = const True
            res `shouldThrow` parserException
        it "disallows connecting negative list element" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "[]" def
                    Graph.addNode top u2 "2" def
                    Graph.connect top (outPortRef u2 []) (InPortRef' $ inPortRef u1 [Port.Arg (-1)])
            let parserException :: Selector AST.ConnectionException
                parserException = const True
            res `shouldThrow` parserException
        it "cleans after removing `foo = a: a` with `4` inside connected to output" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                let loc' = top |>| u1
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode loc' u2 "4" def
                (_, out) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                let referenceConnection = (outPortRef u2 [], inPortRef out [])
                uncurry (connectToInput loc') referenceConnection
                Graph.removeNodes top [u1]
                Graph.withGraph top $ use (userState . breadcrumbHierarchy . BH.children)
            withResult res $ \mapping -> do
                length mapping `shouldBe` 0
        it "removes `foo = a: a`" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.removeNodes top [u1]
                Graph.withGraph top $ use (userState . breadcrumbHierarchy . BH.children)
            withResult res $ \mapping -> do
                length mapping `shouldBe` 0
        it "RHS of `foo = a: a` is Lam" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.withGraph top $ runASTOp $ do
                    isLambda <- ASTRead.rhsIsLambda =<< ASTRead.getASTPointer u1
                    isTrivial <- AST.isTrivialLambda =<< ASTRead.getASTTarget u1
                    return $ isLambda && isTrivial
            withResult res $ \a -> a `shouldBe` True
        it "changes name of a variable in-place" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo" def
                Graph.renameNode top u1 "bar"
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \node -> node ^. Node.name `shouldBe` Just "bar"
        it "retains connection after node rename" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "4" def
                Graph.addNode top u2 "id number1" $ def & position . Position.x .~ 20.0
                Graph.renameNode top u1 "node5"
                graph <- Graph.getGraph top
                expression <- Graph.withGraph top $ runASTOp $ do
                    u2Target <- ASTRead.getASTTarget u2
                    printExpression u2Target
                return (graph, expression)
            withResult res $ \(graph, expression) -> do
                let nodes = graph ^. Graph.nodes
                    Just four = find (\n -> n ^. Node.nodeId == u1) nodes
                four ^. Node.name `shouldBe` Just "node5"
                expression `shouldBe` "id •"
                let connections = graph ^. Graph.connections
                connections `shouldMatchList` [Connection (outPortRef u1 []) (inPortRef u2 [Port.Arg 0])]
        it "changes node expression" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "123" def
                node  <- (^?! Node._ExpressionNode') <$>
                    Graph.setNodeExpression top u1 "456"
                nodes <- Graph.getNodes top
                return (node, nodes)
            withResult res $ \(node, nodes) -> do
                node ^. Node.expression `shouldBe` "456"
                node ^. Node.nodeId     `shouldBe` u1
                nodes `shouldSatisfy` ((== 1) . length)
        it "changes expression to lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                node  <- (^?! Node._ExpressionNode') <$>
                    Graph.setNodeExpression top u1 "a: a"
                nodes <- Graph.getNodes top
                return (node, nodes)
            withResult res $ \(node, nodes) -> do
                node ^. Node.expression `shouldBe` "Ⓕ"
                node ^. Node.code       `shouldBe` "a: a"
                node ^. Node.nodeId     `shouldBe` u1
                node ^. Node.canEnter   `shouldBe` True
                nodes `shouldSatisfy` ((== 1) . length)
        xit "does not allow to change expression to assignment" $ \env -> do
            u1 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "1" def
                    Graph.setNodeExpression top u1 "foo = a: a"
                    Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            let parserException :: Selector Parser.SomeParserException
                parserException = const True
            res `shouldThrow` parserException
        it "changes expression to lambda with node inside" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.setNodeExpression top u1 "a: b: a + b"
                Graph.getGraph (top |>| u1)
            withResult res $ \graph -> do
                let Graph.Graph nodes connections _ _ _ _ = graph
                let [node] = nodes
                node ^. Node.expression `shouldBe` "• + •"
                connections `shouldSatisfy` ((== 3) . length)
        it "changes expression to anonymous node" $ \env -> do
            let code = [qqRawStr|def main:
    «0»print 3.14
    «1»print 3.1414
|]
            res <- evalEmp env $ do
                createLibrary Nothing "TestPath"
                let loc = GraphLocation "TestPath" $ Breadcrumb []
                Graph.loadCode loc code
                [main] <- Graph.getNodes loc
                let loc' = GraphLocation "TestPath" $ Breadcrumb [Definition (main ^. Node.nodeId)]
                Just nodeId <- Graph.withGraph loc' $ do
                    runASTOp $ Graph.getNodeIdForMarker 0
                Graph.setNodeExpression loc' nodeId "print 3.141"
                Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildNode nodeId
            withResult res $ \node -> do
                node ^. Node.expression `shouldBe` "print 3.141"
    describe "dumpAccessors" $ do
        it "foo.bar" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo.bar" def
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u1
                    ASTDeconstruct.dumpAccessors accs
            withResult res $ \(node, accs) -> do
                node `shouldBe` Nothing
                accs `shouldBe` ["foo", "bar"]
        it "baz ---o foo.bar" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo.bar" def
                Graph.addNode top u2 "baz" def
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Self])
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u1
                    (,) <$> ASTRead.getASTVar u2 <*> ASTDeconstruct.dumpAccessors accs
            withResult res $ \(reference, (node, accs)) -> do
                node `shouldBe` Just reference
                accs `shouldBe` ["bar"]
        it "1 ---> foo ---o bar ---o baz" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                Graph.addNode top u2 "foo" def
                connectToInput top (outPortRef u1 []) (inPortRef u2 [Port.Arg 0])
                Graph.addNode top u3 "bar" def
                connectToInput top (outPortRef u2 []) (inPortRef u3 [Port.Self])
                Graph.addNode top u4 "baz" def
                connectToInput top (outPortRef u3 []) (inPortRef u4 [Port.Self])
                Graph.withGraph top $ runASTOp $ do
                    accs <- ASTRead.getASTTarget u4
                    (,) <$> ASTRead.getASTVar u3 <*> ASTDeconstruct.dumpAccessors accs
            withResult res $ \(reference, (node, accs)) -> do
                node `shouldBe` Just reference
                accs `shouldBe` ["baz"]
    describe "show ports on not-yet-typechecked nodes" $ do
        it "shows two input ports on +" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                nodes <- Graph.getNodes top
                return $ find (\node -> node ^. Node.nodeId == u1) nodes
            withResult res $ \(Just plus) -> do
                (plus ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: «2»a + b")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.NotConnected
                    ]
        it "shows self & base port on succ" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "succ" def
                nodes <- Graph.getNodes top
                return $ find (\node -> node ^. Node.expression == "succ") nodes
            withResult res $ \(Just succ') -> do
                (succ' ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "succ")
                    , Port.Port [Port.Self]  "self" TStar Port.NotConnected
                    ]
        it "connects to input port on +" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                Graph.addNode top u2 "1" def
                Graph.getNodes top
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 0])
                Graph.getNodes top
            withResult res $ \nodes -> do
                let Just plus = find (\a -> view Node.nodeId a == u1) nodes
                (plus ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: «2»a + b number1")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.Connected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.NotConnected
                    ]
        it "connects to more than one port" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                Graph.addNode top u3 "2" def
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 0])
                connectToInput top (outPortRef u3 []) (inPortRef u1 [Port.Arg 1])
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                (n ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port [Port.Self]  "self"  TStar Port.NotConnected
                    , Port.Port []           "alias"  TStar (Port.WithDefault $ Expression "func number1 number2")
                    , Port.Port [Port.Arg 0] "number1" TStar Port.Connected
                    , Port.Port [Port.Arg 1] "number2" TStar Port.Connected
                    ]
        it "connects five nodes to func" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            u5 <- mkUUID
            u6 <- mkUUID
            u7 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                Graph.addNode top u3 "2" def
                Graph.addNode top u4 "3" def
                Graph.addNode top u5 "4" def
                Graph.addNode top u6 "5" def
                Graph.addNode top u7 "6" def
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 0])
                connectToInput top (outPortRef u3 []) (inPortRef u1 [Port.Arg 1])
                connectToInput top (outPortRef u4 []) (inPortRef u1 [Port.Arg 2])
                connectToInput top (outPortRef u5 []) (inPortRef u1 [Port.Arg 3])
                connectToInput top (outPortRef u6 []) (inPortRef u1 [Port.Arg 4])
                connectToInput top (outPortRef u7 []) (inPortRef u1 [Port.Arg 5])
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                (n ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port [Port.Self]  "self"    TStar Port.NotConnected
                    , Port.Port []           "alias"   TStar (Port.WithDefault $ Expression "func number1 number2 number3 number4 number5 number6")
                    , Port.Port [Port.Arg 0] "number1" TStar Port.Connected
                    , Port.Port [Port.Arg 1] "number2" TStar Port.Connected
                    , Port.Port [Port.Arg 2] "number3" TStar Port.Connected
                    , Port.Port [Port.Arg 3] "number4" TStar Port.Connected
                    , Port.Port [Port.Arg 4] "number5" TStar Port.Connected
                    , Port.Port [Port.Arg 5] "number6" TStar Port.Connected
                    ]
        it "removes empty port on disconnect" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "func" def
                Graph.addNode top u2 "1" def
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 0])
                Graph.disconnect top (inPortRef u1 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
            withResult res $ \n -> do
                (n ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []          "alias"  TStar (Port.WithDefault $ Expression "func")
                    , Port.Port [Port.Self] "self"  TStar Port.NotConnected
                    ]
        it "disconnect first connection when two nodes connected" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "first" def
                Graph.addNode top u2 "second" def
                Graph.addNode top u3 "func" def
                connectToInput top (outPortRef u1 []) (inPortRef u3 [Port.Arg 0])
                connectToInput top (outPortRef u2 []) (inPortRef u3 [Port.Arg 1])
                Graph.disconnect top (inPortRef u3 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u3
            withResult res $ \n -> do
                (n ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias"   TStar (Port.WithDefault $ Expression "func _ second1")
                    , Port.Port [Port.Self]  "self"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 0] "_"       TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "second1" TStar Port.Connected
                    ]
        it "disconnects first connection when three connected" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            u4 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "first" def
                Graph.addNode top u2 "second" def
                Graph.addNode top u3 "third" def
                Graph.addNode top u4 "func" def
                connectToInput top (outPortRef u1 []) (inPortRef u4 [Port.Arg 0])
                connectToInput top (outPortRef u2 []) (inPortRef u4 [Port.Arg 1])
                connectToInput top (outPortRef u3 []) (inPortRef u4 [Port.Arg 2])
                Graph.disconnect top (inPortRef u4 [Port.Arg 0])
                Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u4
            withResult res $ \n -> do
                (n ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias"   TStar (Port.WithDefault $ Expression "func _ second1 third1")
                    , Port.Port [Port.Self]  "self"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 0] "_"       TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "second1" TStar Port.Connected
                    , Port.Port [Port.Arg 2] "third1"  TStar Port.Connected
                    ]
        it "connects id to itself" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "id" def
                connectToInput top (outPortRef u1 []) (inPortRef u1 [Port.Arg 0])
                Graph.getConnections top
            res `shouldMatchList` [Connection (OutPortRef (convert u1) []) (InPortRef (convert u1) [Port.Arg 0])]
    describe "port manipulation" $ do
        let buildInputEdge' loc nid = Graph.withGraph loc $ runASTOp $ GraphBuilder.buildInputSidebar nid
        it "adds port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' $ outPortRef input [Port.Projection 1]
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                let referenceConnection = Connection (outPortRef input [Port.Projection 0]) (inPortRef output [])
                return (inputEdge, defFoo, connections, referenceConnection)
            withResult res $ \(inputEdge, defFoo, connections, referenceConnection) -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "a" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 1] "b" TStar Port.NotConnected)
                    ]
                (defFoo ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: a")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.NotConnected
                    ]
                connections `shouldMatchList` [referenceConnection]
        it "adds port with a specified name" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPortWithConnections loc' (outPortRef input [Port.Projection 1]) (Just "zzz") []
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                let referenceConnection = Connection (outPortRef input [Port.Projection 0]) (inPortRef output [])
                return (inputEdge, defFoo, connections, referenceConnection)
            withResult res $ \(inputEdge, defFoo, connections, referenceConnection) -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "a" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 1] "zzz" TStar Port.NotConnected)
                    ]
                (defFoo ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: zzz: a")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "zzz"    TStar Port.NotConnected
                    ]
                connections `shouldMatchList` [referenceConnection]
        it "adds port at the first position" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 0])
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                let referenceConnection = Connection (outPortRef input [Port.Projection 1]) (inPortRef output [])
                return (inputEdge, defFoo, connections, referenceConnection)
            withResult res $ \(inputEdge, defFoo, connections, referenceConnection) -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "b" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 1] "a" TStar Port.NotConnected)
                    ]
                (defFoo ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "b: a: a")
                    , Port.Port [Port.Arg 0] "b"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "a"    TStar Port.NotConnected
                    ]
                connections `shouldMatchList` [referenceConnection]
        it "adds two ports" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' $ outPortRef input [Port.Projection 1]
                Graph.addPort loc' $ outPortRef input [Port.Projection 2]
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                return (inputEdge, defFoo)
            withResult res $ \(inputEdge, defFoo) -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "a" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 1] "b" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 2] "c" TStar Port.NotConnected)
                    ]
                (defFoo ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: c: a")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 2] "c"    TStar Port.NotConnected
                    ]
        it "adds port on literal lambda" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a + b" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' $ outPortRef input [Port.Projection 2]
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                return (inputEdge, defFoo, connections)
            withResult res $ \(inputEdge, defFoo, connections) -> do
                inputEdge ^.. Node.inputEdgePorts . traverse . traverse `shouldMatchList` [
                      Port.Port [Port.Projection 0] "a" TStar Port.NotConnected
                    , Port.Port [Port.Projection 1] "b" TStar Port.NotConnected
                    , Port.Port [Port.Projection 2] "c" TStar Port.NotConnected
                    ]
                (defFoo ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: c: «2»a + b")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 2] "c"    TStar Port.NotConnected
                    ]
                connections `shouldSatisfy` ((== 3) . length)
        it "connects to added port" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                Graph.addNode top u2 "func" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' $ outPortRef input [Port.Projection 1]
                connectToInput top (outPortRef u2 []) (inPortRef u1 [Port.Arg 1])
                node <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections top
                return (node, connections)
            withResult res $ \(node, connections) -> do
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: b: a _ func1")
                    , Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected
                    , Port.Port [Port.Arg 1] "b"    TStar Port.Connected
                    ]
                connections `shouldMatchList` [
                      Connection (outPortRef u2 []) (inPortRef u1 [Port.Arg 1])
                    ]
        it "removes port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.removePort loc' $ outPortRef input [Port.Projection 1]
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                return (inputEdge, defFoo)
            withResult res $ \(inputEdge, node) -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "a" TStar Port.NotConnected)
                    ]
                (node ^.. Node.inPorts . traverse) `shouldMatchList` [
                      Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: a")
                    , Port.Port [Port.Arg 0] "a" TStar Port.NotConnected
                    ]
        it "renames port" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: a" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.renamePort loc' (outPortRef input [Port.Projection 0]) "foo"
                Graph.renamePort loc' (outPortRef input [Port.Projection 1]) "bar"
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
                (inputEdge ^. Node.inputEdgePorts) `shouldMatchList` [
                      LabeledTree def (Port.Port [Port.Projection 0] "foo" TStar Port.NotConnected)
                    , LabeledTree def (Port.Port [Port.Projection 1] "bar" TStar Port.NotConnected)
                    ]
        it "changes ports order" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: b: c: d: a" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.movePort loc' (outPortRef input [Port.Projection 0]) 2
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
                inputEdge ^.. Node.inputEdgePorts . traverse . traverse `shouldMatchList` [
                      Port.Port [Port.Projection 0] "b" TStar Port.NotConnected
                    , Port.Port [Port.Projection 1] "c" TStar Port.NotConnected
                    , Port.Port [Port.Projection 2] "a" TStar Port.NotConnected
                    , Port.Port [Port.Projection 3] "d" TStar Port.NotConnected
                    ]
        it "connects to added port inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                Graph.addNode loc' u2 "func" def
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                connectToInput loc' (outPortRef input [Port.Projection 1]) (inPortRef u2 [Port.Arg 0])
                inputEdge <- buildInputEdge' loc' input
                connections <- Graph.getConnections loc'
                let referenceConnections = [
                        Connection (outPortRef input [Port.Projection 0]) (inPortRef output [])
                      , Connection (outPortRef input [Port.Projection 1]) (inPortRef u2     [Port.Arg 0])
                      ]
                return (inputEdge, connections, referenceConnections)
            withResult res $ \(inputEdge, connections, referenceConnections) -> do
                inputEdge ^.. Node.inputEdgePorts . traverse . traverse `shouldMatchList` [
                      Port.Port [Port.Projection 0] "a" TStar Port.NotConnected
                    , Port.Port [Port.Projection 1] "b" TStar Port.NotConnected
                    ]
                connections `shouldMatchList` referenceConnections
        it "does not allow to remove All port" $ \env -> do
            u1 <- mkUUID
            let res = evalEmp env $ do
                    Graph.addNode top u1 "foo = a: a" def
                    let loc' = top |>| u1
                    (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                    Graph.removePort loc' (outPortRef input [])
            let cannotRemovePortException :: Selector CannotRemovePortException
                cannotRemovePortException = const True
            res `shouldThrow` cannotRemovePortException
        it "removes port that is connected inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                Graph.addNode loc' u2 "func" def
                (input, output) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                connectToInput loc' (outPortRef input [Port.Projection 1]) (inPortRef u2 [Port.Arg 0])
                Graph.removePort loc' (outPortRef input [Port.Projection 1])
                inputEdge <- buildInputEdge' loc' input
                defFoo <- Graph.withGraph top $ runASTOp $ GraphBuilder.buildNode u1
                connections <- Graph.getConnections loc'
                nodeIds <- (map (^. Node.nodeId)) <$> (Graph.withGraph loc' $ runASTOp $ GraphBuilder.buildNodes)
                let referenceConnections = [Connection (outPortRef input [Port.Projection 0]) (inPortRef output [])]
                return (inputEdge, defFoo, connections, referenceConnections, nodeIds)
            withResult res $ \(inputEdge, _node, connections, referenceConnections, nodeIds) -> do
                inputEdge ^.. Node.inputEdgePorts . traverse . traverse `shouldMatchList` [
                      Port.Port [Port.Projection 0] "a" TStar Port.NotConnected
                    ]
                -- FIXME[MK]: problem with markers showing up randomly
                {-node ^.. Node.inPorts . traverse `shouldMatchList` [-}
                      {-Port.Port []           "alias" TStar (Port.WithDefault $ Expression "a: node2 = func b\n   a")-}
                    {-, Port.Port [Port.Arg 0] "a"    TStar Port.NotConnected-}
                    {-]-}
                connections `shouldMatchList` referenceConnections
                nodeIds `shouldMatchList` [u2]
        it "can build input edge after connecting input edge to self" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc' = top |>| u1
                (input, _) <- Graph.withGraph loc' $ runASTOp GraphBuilder.getEdgePortMapping
                Graph.addPort loc' (outPortRef input [Port.Projection 1])
                Graph.addNode loc' u2 "succ" def
                connectToInput loc' (outPortRef input [Port.Projection 1]) (inPortRef u2 [Port.Self])
                inputEdge <- buildInputEdge' loc' input
                return inputEdge
            withResult res $ \inputEdge -> do
              inputEdge ^.. Node.inputEdgePorts . traverse . traverse `shouldMatchList` [
                    Port.Port [Port.Projection 0] "a" TStar Port.NotConnected
                  , Port.Port [Port.Projection 1] "b" TStar Port.NotConnected
                  ]
    describe "node sequence" $ do
        it "adds one node to sequence" $ \env -> do
            u1 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" def
                seq <- Graph.withGraph top $ runASTOp $ GraphBuilder.getNodeIdSequence
                return seq
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u1]
        it "adds three nodes in line" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (Position.fromTuple (10, 10)) False def
                Graph.addNode top u2 "2" $ NodeMeta (Position.fromTuple (20, 10)) False def
                Graph.addNode top u3 "3" $ NodeMeta (Position.fromTuple (30, 10)) False def
                seq <- Graph.withGraph top $ runASTOp $ GraphBuilder.getNodeIdSequence
                return seq
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u1, u2, u3]
        it "adds three nodes in reverse order" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (Position.fromTuple (30, 10)) False def
                Graph.addNode top u2 "2" $ NodeMeta (Position.fromTuple (20, 10)) False def
                Graph.addNode top u3 "3" $ NodeMeta (Position.fromTuple (10, 10)) False def
                seq <- Graph.withGraph top $ runASTOp $ GraphBuilder.getNodeIdSequence
                return seq
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u2, u1]
        it "updates sequence after node removal" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (Position.fromTuple (10, 30)) False def
                Graph.addNode top u2 "2" $ NodeMeta (Position.fromTuple (10, 20)) False def
                Graph.addNode top u3 "3" $ NodeMeta (Position.fromTuple (10, 10)) False def
                Graph.removeNodes top [u2]
                seq <- Graph.withGraph top $ runASTOp $ GraphBuilder.getNodeIdSequence
                return seq
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u1]
        it "updates sequence after node meta update" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "1" $ NodeMeta (Position.fromTuple (10, 10)) False def
                Graph.addNode top u2 "2" $ NodeMeta (Position.fromTuple (10, 20)) False def
                Graph.addNode top u3 "3" $ NodeMeta (Position.fromTuple (10, 30)) False def
                Graph.setNodeMeta top u3 $ NodeMeta (Position.fromTuple (10, 10)) False def
                Graph.setNodeMeta top u2 $ NodeMeta (Position.fromTuple (20, 30)) False def
                Graph.setNodeMeta top u1 $ NodeMeta (Position.fromTuple (30, 20)) False def
                seq <- Graph.withGraph top $ runASTOp $ GraphBuilder.getNodeIdSequence
                return seq
            withResult res $ \nodeSeq -> do
                nodeSeq `shouldMatchList` [u3, u2, u1]
        it "adds one node inside lambda" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc = top |>| u1
                Graph.addNode loc u2 "4" def
                Graph.withGraph loc $ runASTOp $ GraphBuilder.getNodeIdSequence
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` [u2]
        it "adds two nodes inside" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            u3 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "foo = a: a" def
                let loc = top |>| u1
                Graph.addNode loc u2 "4" $ NodeMeta (Position.fromTuple (10, 10)) False def
                Graph.addNode loc u3 "6" $ NodeMeta (Position.fromTuple (10, 20)) False def
                Graph.withGraph loc $ runASTOp $ GraphBuilder.getNodeIdSequence
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` [u2, u3]
        it "adds one node inside and removes it" $ \env -> do
            u1 <- mkUUID
            u2 <- mkUUID
            res <- evalEmp env $ do
                Graph.addNode top u1 "a: a" def
                let loc = top |>| u1
                Graph.addNode loc u2 "1" def
                Graph.removeNodes loc [u2]
                Graph.withGraph loc $ runASTOp $ GraphBuilder.getNodeIdSequence
            withResult res $ \idSeq -> do
                idSeq    `shouldBe` []
