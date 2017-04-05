{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module EmpireUtils (
      runEmp
    , evalEmp
    , runEmp'
    , runEmpire
    , graphIDs
    , extractGraph
    , withResult
    , top
    , (|>)
    , (|>-)
    , mkUUID
    , withChannels
    , emptyGraphLocation
    , connectToInput
    ) where

import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (newTChan)
import           Control.Exception             (Exception, bracket)
import           Data.Coerce                   (coerce)
import qualified Data.Map                      as Map
import           Data.Reflection               (Given (..), give)
import           Data.UUID                     (UUID, nil)
import           Data.UUID.V4                  (nextRandom)
import           Empire.API.Data.Breadcrumb    (Breadcrumb(..), BreadcrumbItem(Lambda, Arg))
import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation(..))
import           Empire.API.Data.Port          (Port)
import qualified Empire.API.Data.Port          as Port
import           Empire.API.Data.PortRef       (AnyPortRef(InPortRef'), InPortRef, OutPortRef)
import           Empire.API.Data.Node          (NodeId, nodeId)
import qualified Empire.Commands.Graph         as Graph (connect, getNodes)
import           Empire.Commands.Library       (createLibrary, listLibraries, withLibrary)
import           Empire.Commands.Project       (createProject, listProjects)
import           Empire.Data.AST               ()
import           Empire.Data.Graph             (AST, ASTState (..), Graph)
import qualified Empire.Data.Library           as Library (body)
import           Empire.Empire                 (CommunicationEnv (..), Empire, Env, Error, InterpreterEnv (..), runEmpire)
import           Luna.IR                       (AnyExpr, Link')
import           Prologue                      hiding (mapping, toList, (|>))

import           Test.Hspec                    (expectationFailure)


runEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (Either Error a, Env)
runEmp env act = runEmpire env def $ do
    (pid, _) <- createProject Nothing "project1"
    (lid, _) <- createLibrary pid (Just "lib1") "/libs/lib1"
    let toLoc = GraphLocation pid lid
    give (toLoc $ Breadcrumb []) act

evalEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (Either Error a)
evalEmp env act = fst <$> runEmp env act

runEmp' :: CommunicationEnv -> Env -> Graph ->
              (Given GraphLocation => Empire a) -> IO (Either Error a, Env)
runEmp' env st newGraph act = runEmpire env st $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    withLibrary pid lid $ Library.body .= newGraph
    let toLoc = GraphLocation pid lid
    give (toLoc $ Breadcrumb []) act

graphIDs :: GraphLocation -> Empire [NodeId]
graphIDs loc = do
    nodes <- Graph.getNodes loc
    let ids = map (^. nodeId) nodes
    return ids

extractGraph :: InterpreterEnv -> Graph
extractGraph (InterpreterEnv _ _ _ g _) = g

data DummyB = DummyB deriving (Show, Exception)

-- DummyB is only here because expectationFailure returns IO () and we need to
-- return arbitrary type from lambda
withResult :: Either String a -> (a -> IO b) -> IO b
withResult (Left err)  _   = expectationFailure err >> throwM DummyB
withResult (Right res) act = act res

top :: Given GraphLocation => GraphLocation
top = given

infixl 5 |>
(|>) :: GraphLocation -> NodeId -> GraphLocation
(|>) (GraphLocation pid lid bc) nid = GraphLocation pid lid $ coerce $ (++ [Lambda nid]) $ coerce bc

infixl 5 |>-
(|>-) :: GraphLocation -> (NodeId, Int) -> GraphLocation
(|>-) (GraphLocation pid lid bc) it = GraphLocation pid lid $ Breadcrumb $ (++ [uncurry Arg it]) $ coerce bc

withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = atomically $ CommunicationEnv <$> newTChan <*> newTChan

emptyGraphLocation :: GraphLocation
emptyGraphLocation = GraphLocation nil 0 $ Breadcrumb []

mkUUID :: IO UUID
mkUUID = nextRandom

connectToInput :: GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connectToInput loc outPort inPort = Graph.connect loc outPort (InPortRef' inPort)
