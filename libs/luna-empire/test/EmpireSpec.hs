{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module EmpireSpec (spec) where

import           Prologue hiding (toList)
import           Control.Exception (bracket)
import           Control.Monad.Except (throwError)
import           Data.Foldable (toList)
import           Data.List (find, sort, stripPrefix)
import qualified Data.Map as Map
import           Data.UUID (nil)
import           Data.UUID.V4 (nextRandom)
import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Commands.GraphUtils   as GraphUtils
import           Empire.API.Data.Node         as Node
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Port
import           Empire.API.Data.GraphLocation
import           Empire.API.Data.Breadcrumb   as Breadcrumb
import           Empire.API.Data.PortRef      (AnyPortRef(..), InPortRef (..), OutPortRef (..), srcNodeId)
import           Empire.API.Data.TypeRep
import           Empire.API.Data.ValueType
import           Empire.Commands.AST
import           Empire.Commands.Graph        as Graph
import           Empire.Commands.Library
import           Empire.Commands.Project
import           Empire.Commands.Typecheck    as Typecheck
import           Empire.Data.AST
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.Empire
import           Control.Concurrent.STM.TChan (TChan, newTChan)
import           Control.Concurrent.STM       (atomically)
import           Old.Luna.Pretty.GraphViz     (renderAndOpen, toGraphViz)

import           Test.Hspec (around, describe, expectationFailure, it,
                             shouldBe, shouldContain, shouldSatisfy, shouldMatchList)


runGraph env act = runEmpire env def $ do
    (pid, _) <- createProject Nothing "dupa"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"
    let toLoc = GraphLocation pid lid
    act toLoc

-- runGraph' :: CommunicationEnv -> Env -> ((Breadcrumb -> GraphLocation) -> Command Graph a) -> IO (Either Error a, Env)
runGraph' env graph act = runEmpire env graph $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    let toLoc = GraphLocation pid lid
    act toLoc

-- runGraph'' :: _ -> _ -> Graph -> _ -> _
runGraph'' env st newGraph act = runEmpire env st $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    withLibrary pid lid $ body .= newGraph
    let toLoc = GraphLocation pid lid
    act toLoc

graphIDs loc = do
  nodes <- view Graph.nodes <$> Graph.getGraph loc
  let ids = map (^. nodeId) nodes
  return ids

exGraph (InterpreterEnv _ _ _ g _) = g

excludeEdges :: [Node] -> [Node]
excludeEdges = filter (not . isEdge . view nodeType)
    where
        isEdge InputEdge{}  = True
        isEdge OutputEdge{} = True
        isEdge _            = False

spec = around withChannels $
    describe "luna-empire" $ do
        it "descends into `def foo` and asserts null list of nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, state) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                topLevel <- graphIDs loc
                n1Level <- graphIDs loc'
                return (topLevel, n1Level)
            case res of
                Left err -> expectationFailure err
                Right ids -> do
                    u1 `shouldSatisfy` (`elem` (fst ids))
        it "adds node" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                n1 <- Graph.addNode loc u1 "def foo" def
                return n1
            case res of
                Left err -> expectationFailure err
                Right _ -> return ()
        it "makes connection to output edge" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef (n2 ^. nodeId) All, InPortRef out (Arg 0))
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            case res of
                Left err -> expectationFailure err
                Right (conn, connections) -> do
                    connections `shouldSatisfy` ((== 1) . length)
                    head connections `shouldBe` conn
        it "connects input edge to succ (Self)" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input All, InPortRef (n2 ^. nodeId) Self)
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            case res of
                Left err -> expectationFailure err
                Right (conn, connections) -> do
                    connections `shouldSatisfy` ((== 2) . length)
                    connections `shouldContain` [conn]
        it "connects input edge to dummy node (Arg 0)" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input All, InPortRef (n2 ^. nodeId) (Arg 0))
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            case res of
                Left err -> expectationFailure err
                Right (conn, connections) -> do
                    connections `shouldSatisfy` ((== 2) . length)
                    connections `shouldContain` [conn]
        it "has proper connection inside `def foo`" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                conns <- view Graph.connections <$> Graph.getGraph loc'
                Just edges <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                return (conns, edges)
            case res of
                Left err -> expectationFailure err
                Right (connections, (inputEdge, outputEdge)) -> do
                    connections `shouldMatchList` [(OutPortRef inputEdge (Projection 0), InPortRef outputEdge (Arg 0))]
        it "shows connection inside lambda" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            u3 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "4" def
                n3 <- Graph.addNode loc' u3 "succ" def
                let referenceConnection = (OutPortRef (n2 ^. nodeId) All, InPortRef (n3 ^. nodeId) Self)
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            case res of
                Left err -> expectationFailure err
                Right (ref, connections) -> do
                    connections `shouldSatisfy` ((== 2) . length)
                    connections `shouldContain` [ref]
        it "creates two nested lambdas and a node inside" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            u3 <- nextRandom
            (res, state) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "def bar" def
                let loc'' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1, Breadcrumb.Lambda u2]
                n3 <- Graph.addNode loc'' u3 "4" def
                graphIDs loc''
            case res of
                Left err -> expectationFailure err
                Right ids -> do
                    u3 `shouldSatisfy` (`elem` ids)
                    u1 `shouldSatisfy` (`notElem` ids)
                    u2 `shouldSatisfy` (`notElem` ids)
        it "cannot enter lambda applied to value" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                n1 <- Graph.addNode loc u1 "def foo" def
                n2 <- Graph.addNode loc u2 "4" def
                Graph.connect loc (OutPortRef u2 All) (InPortRef u1 (Arg 0))
                Graph.getGraph loc
            case res of
                Left err -> expectationFailure err
                Right g -> do
                    let Just lambdaNode = find ((== u1) . Node._nodeId) $ Graph._nodes g
                    lambdaNode ^. Node.canEnter `shouldBe` False
        it "has no null node inside `def foo`" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                n1 <- Graph.addNode loc u1 "def foo" def
                view Graph.nodes <$> Graph.getGraph (mkLoc $ Breadcrumb [Breadcrumb.Lambda u1])
            case res of
                Left err -> expectationFailure err
                Right (excludeEdges -> ids) -> do
                    ids `shouldSatisfy` null
        it "int literal has no nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "4" def
                graphIDs loc'
            case res of
                Left err -> case stripPrefix "cannot enter node Just" err of
                    Just _ -> return ()
                    _      -> expectationFailure err
                Right _  -> expectationFailure "should throw"
        it "properly typechecks input nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                Graph.addNode loc u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = loc
                withLibrary pid lid (use $ body)
            case res of
                Left err -> expectationFailure err
                Right g -> do
                    (res', (exGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                        Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                    (Right foo,_) <- runGraph'' env st g' $ \mkLoc -> do
                        Graph.getGraph $ mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                    let nodes' = foo ^. Graph.nodes
                        Just input = find ((== "inputEdge") . Node._name) nodes'
                        ports' = toList $ input ^. ports
                        types = map (view valueType) ports'
                    types `shouldMatchList` [TypeIdent (TCons "Int" []), TypeIdent (TCons "Int" [])]
        it "properly typechecks output nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                Graph.addNode loc u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = loc
                withLibrary pid lid (use $ body)
            case res of
                Left err -> expectationFailure err
                Right g -> do
                    (res', (exGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                        Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                    (Right foo,_) <- runGraph'' env st g' $ \mkLoc -> do
                        Graph.getGraph $ mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                    let nodes' = foo ^. Graph.nodes
                        Just output' = find ((== "outputEdge") . Node._name) nodes'
                        ports' = toList $ output' ^. ports
                        types = map (view valueType) ports'
                    types `shouldBe` [TypeIdent (TCons "Int" [])]
        it "adds lambda nodeid to node mapping" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                Graph.addNode loc u1 "-> $a $b a + b" def
                withGraph loc $ use nodeMapping
            case res of
                Left err -> expectationFailure err
                Right (toList -> mapping) -> do
                    let isLambdaNode n = case n of
                            AnonymousNode _ -> True
                            _               -> False
                        lambdaNodes = filter isLambdaNode mapping
                    lambdaNodes `shouldSatisfy` (not . null)
        it "puts + inside plus lambda" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                Graph.addNode loc u1 "-> $a $b a + b" def
                view Graph.nodes <$> Graph.getGraph loc'
            case res of
                Left err -> expectationFailure err
                Right (excludeEdges -> nodes) -> do
                    nodes `shouldSatisfy` ((== 1) . length)
                    head nodes `shouldSatisfy` (\a -> a ^. nodeType . expression == "a + b")
        it "places connections between + node and output" $ \env -> do
          u1 <- nextRandom
          (res, _) <- runGraph env $ \mkLoc -> do
              let loc = mkLoc $ Breadcrumb []
                  loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
              Graph.addNode loc u1 "-> $a $b a + b" def
              view Graph.connections <$> Graph.getGraph loc'
          case res of
              Left err -> expectationFailure err
              Right conns -> do
                  conns `shouldSatisfy` ((== 1) . length)

withChannels :: (CommunicationEnv -> IO ()) -> IO ()
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = atomically $ CommunicationEnv <$> newTChan <*> newTChan
