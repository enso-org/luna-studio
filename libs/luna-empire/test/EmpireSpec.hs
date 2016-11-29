{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module EmpireSpec (spec) where

import           Prologue
import           Control.Exception (bracket)
import           Control.Monad.Except (throwError)
import           Data.List (sort)
import           Data.UUID (nil)
import           Data.UUID.V4 (nextRandom)
import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph        as Graph
import           Empire.API.Data.Node
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Port
import           Empire.API.Data.GraphLocation
import           Empire.API.Data.Breadcrumb   as Breadcrumb
import           Empire.API.Data.PortRef      (AnyPortRef(..), InPortRef (..), OutPortRef (..))
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

import           Test.Hspec (around, describe, expectationFailure, it, shouldSatisfy)


spec = around withChannels $
    describe "luna-empire" $ do
        it "descends into `def foo` and asserts null list of nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, state) <- runEmpire env def $ do
                (pid, _) <- createProject Nothing "project1"
                (lid, _) <- createLibrary pid (Just "lib1") "/libs/lib1"

                let loc = GraphLocation pid lid $ Breadcrumb []
                let loc' = GraphLocation pid lid $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                let graphIDs loc = do
                        nodes <- view Graph.nodes <$> Graph.getGraph loc
                        let ids = map (^. nodeId) nodes
                        return ids
                topLevel <- graphIDs loc
                n1Level <- graphIDs loc'
                return (topLevel, n1Level)
            case res of
                Left err -> expectationFailure err
                Right ids -> u1 `shouldSatisfy` (`elem` (fst ids))

withChannels :: (CommunicationEnv -> IO ()) -> IO ()
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = atomically $ CommunicationEnv <$> newTChan <*> newTChan

test :: Empire Graph
test = do
    (pid, _) <- createProject Nothing "project1"
    (lid, _) <- createLibrary pid (Just "lib1") "/libs/lib1"

    let loc = GraphLocation pid lid $ Breadcrumb []

    u1 <- liftIO $ nextRandom
    u2 <- liftIO $ nextRandom
    u3 <- liftIO $ nextRandom
    n1 <- Graph.addNode loc u1 "foo" def
    n2 <- Graph.addNode loc u2 "bar" def
    n3 <- Graph.addNode loc u3 "baz" def

    let loc' = GraphLocation pid lid $ Breadcrumb [Breadcrumb.Lambda u1]

    u4 <- liftIO $ nextRandom
    n4 <- Graph.addNode loc' u4 "cokolwiek" def

    let graphIDs loc = do
            nodes <- view Graph.nodes <$> Graph.getGraph loc
            let ids = map (^. nodeId) nodes
            return ids

    ids <- graphIDs loc
    when (u1 `notElem` ids || u2 `notElem` ids || u3 `notElem` ids) $ throwError "ERROR"

    ids <- graphIDs loc'
    when (u4 `notElem` ids) $ throwError "ERROR2"
    withLibrary pid lid (use $ body)

main :: IO ()
main = do
    dummyNotif <- atomically newTChan
    dummyTC    <- atomically newTChan
    (g, st)    <- runEmpire (CommunicationEnv dummyNotif dummyTC) def test
    case g of
        Left err -> putStrLn err
        Right g  -> do
            res <- runEmpire (CommunicationEnv dummyNotif dummyTC) (InterpreterEnv def def def g def) $ Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
            return ()
