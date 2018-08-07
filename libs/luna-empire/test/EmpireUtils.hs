{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

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
    , (|>=)
    , mkUUID
    , withChannels
    , emptyGraphLocation
    , connectToInput
    , outPortRef
    , inPortRef
    ) where

import           Control.Concurrent.MVar       (newEmptyMVar)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (newTChan)
import           Control.Exception             (Exception, bracket)
import           Data.Coerce                   (coerce)
import qualified Data.Map                      as Map
import           Data.Reflection               (Given (..), give)
import           Data.UUID                     (UUID, nil)
import           Data.UUID.V4                  (nextRandom)
import qualified Empire.Commands.Graph         as Graph (addNode, connect, getNodes, loadCode)
import           Empire.Commands.Library       (createLibrary, listLibraries, withLibrary)
import           Empire.Data.AST               ()
import           Empire.Data.Graph             (CommandState(..), ClsGraph, Graph, defaultPMState, userState)
import qualified Empire.Data.Library           as Library (body, path)
import           Empire.Empire                 (CommunicationEnv (..), Empire, Env, Error, InterpreterEnv (..), runEmpire)
import           Empire.Prelude                hiding (mapping, toList, (|>))
import           LunaStudio.Data.Breadcrumb    (Breadcrumb (..), BreadcrumbItem (Arg, Definition, Lambda))
import           LunaStudio.Data.Connection    (Connection)
import           LunaStudio.Data.GraphLocation (GraphLocation (..))
import           LunaStudio.Data.Node          (ExpressionNode, NodeId, nodeId)
import qualified LunaStudio.Data.Node          as Node
import           LunaStudio.Data.NodeLoc       (NodeLoc (..))
import           LunaStudio.Data.Port          (Port)
import qualified LunaStudio.Data.Port          as Port
import qualified LunaStudio.Data.PortRef       as PortRef
import           LunaStudio.Data.PortRef       (AnyPortRef(InPortRef'), InPortRef(..), OutPortRef(..))

import           Test.Hspec                    (expectationFailure)


runEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp env act = do
    pm <- defaultPMState
    runEmpire env (CommandState pm def) $ do
        _ <- createLibrary (Just "/TestFile") "/TestFile"
        let toLoc = GraphLocation "/TestFile"
        Graph.loadCode (toLoc (Breadcrumb [])) "def main:\n    None"
        [node] <- Graph.getNodes (toLoc (Breadcrumb []))
        -- uuid <- mkUUID
        -- Graph.addNode (toLoc $ Breadcrumb []) uuid "def main" def
        -- [node] <- Graph.getNodes (toLoc (Breadcrumb []))
        give (toLoc $ Breadcrumb [Definition (node ^. Node.nodeId)]) act

evalEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO a
evalEmp env act = fst <$> runEmp env act

runEmp' :: CommunicationEnv -> CommandState Env -> ClsGraph ->
              (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp' env st newGraph act = runEmpire env st $ do
    lib <- head <$> listLibraries
    let path = lib ^. Library.path
    withLibrary path $ userState . Library.body .= newGraph
    let toLoc = GraphLocation path
    give (toLoc $ Breadcrumb []) act

graphIDs :: GraphLocation -> Empire [NodeId]
graphIDs loc = do
    nodes <- Graph.getNodes loc
    let ids = fmap (^. nodeId) nodes
    return ids

extractGraph :: InterpreterEnv -> ClsGraph
extractGraph (InterpreterEnv g _ _ _ _ _) = error "extractGraph"

withResult :: a -> (a -> IO b) -> IO b
withResult res act = act res

top :: Given GraphLocation => GraphLocation
top = given

infixl 5 |>
(|>) :: GraphLocation -> NodeId -> GraphLocation
(|>) (GraphLocation file bc) nid = GraphLocation file $ coerce $ (<> [Lambda nid]) $ coerce bc

infixl 5 |>-
(|>-) :: GraphLocation -> (NodeId, Int) -> GraphLocation
(|>-) (GraphLocation file bc) it = GraphLocation file $ Breadcrumb $ (<> [uncurry Arg it]) $ coerce bc

infixl 5 |>=
(|>=) :: GraphLocation -> NodeId -> GraphLocation
(|>=) (GraphLocation file bc) it = GraphLocation file $ Breadcrumb $ (<> [Definition it]) $ coerce bc

withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = CommunicationEnv <$> atomically newTChan <*> newEmptyMVar <*> newEmptyMVar

emptyGraphLocation :: GraphLocation
emptyGraphLocation = GraphLocation "" $ Breadcrumb []

mkUUID :: MonadIO m => m UUID
mkUUID = liftIO nextRandom

connectToInput :: GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connectToInput loc outPort inPort = Graph.connect loc outPort (InPortRef' inPort)

outPortRef :: NodeId -> Port.OutPortId -> OutPortRef
outPortRef = OutPortRef . NodeLoc def

inPortRef :: NodeId -> Port.InPortId -> InPortRef
inPortRef = InPortRef . NodeLoc def
