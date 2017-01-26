{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.ResultSaver where

import           Control.Monad                     (forever)
import           Control.Monad.Except              (throwError)
import           Control.Monad.State               (StateT, evalStateT)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (unpack)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import qualified Data.ByteString.Lazy              as BS
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID
import           Prelude                           (error, undefined)
import           Prologue
import           Text.Groom                        (groom)

import qualified Empire.API.Control.EmpireStarted  as EmpireStarted
import           Empire.API.Data.Breadcrumb        (Breadcrumb (..))
import qualified Empire.API.Data.Graph             as Graph
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.Project           (ProjectId)
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Graph.NodesUpdate      as NodesUpdate
import qualified Empire.API.Project.ExportProject  as ExportProject
import qualified Empire.API.Project.ImportProject  as ImportProject
import           Empire.API.Request                (Request (..))
import qualified Empire.API.Response               as Response
import qualified Empire.API.Topic                  as Topic
import qualified Empire.Commands.Library           as Library
import qualified Empire.Commands.Project           as Project
import qualified Empire.Empire                     as Empire
import qualified Empire.Handlers                   as Handlers
import           Empire.ResultSaver.Env            (ResultSaverEnv)
import qualified Empire.ResultSaver.Env            as Env
import           Empire.ResultSaver.ProjectDump    (ProjectDump (..))
import qualified Empire.Utils                      as Utils

import qualified System.Log.MLogger                as Logger
import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Flag                 as Flag
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic                (Topic)
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import           ZMQ.Bus.Trans                     (BusT (..))
import qualified ZMQ.Bus.Trans                     as Bus

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendToBus :: (Topic.MessageTopic a, Bin.Binary a) => a -> StateT ResultSaverEnv BusT ()
sendToBus msg = void $ lift $ BusT $ Bus.send Flag.Enable $ Message.Message (Topic.topic msg) $ toStrict . Bin.encode $ msg

save :: BusEndPoints -> GraphLocation -> IO (Either Bus.Error ProjectDump)
save endPoints graphLocation = Bus.runBus endPoints $ do
    let topics = ["empire."]
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display True) endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runSave graphLocation) def

runSave :: GraphLocation -> StateT ResultSaverEnv BusT ProjectDump
runSave graphLocation = do
    Env.graphLocation .= graphLocation
    uuid <- liftIO $ UUID.nextRandom
    let request = Request uuid $ GetProgram.Request graphLocation
    sendToBus request
    Env.state .= Env.ProgramRequested uuid
    untilFinished handleMessage

importAndSave :: BusEndPoints -> Text -> IO (Either Bus.Error ProjectDump)
importAndSave endPoints projectData = Bus.runBus endPoints $ do
    let topics = ["empire."]
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display True) endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runImportAndSave projectData) def

runImportAndSave :: Text -> StateT ResultSaverEnv BusT ProjectDump
runImportAndSave projectData = do
    uuid <- liftIO $ UUID.nextRandom
    let request = Request uuid $ ImportProject.Request projectData
    sendToBus request
    Env.state .= Env.ImportRequested uuid
    untilFinished handleMessage

untilFinished :: StateT ResultSaverEnv BusT () -> StateT ResultSaverEnv BusT ProjectDump
untilFinished act = do
  st <- use Env.state
  case st of
    Env.Finished dump -> return dump
    Env.Error errMsg  -> error $ "Invalid response: " <> errMsg
    _                 -> act >> untilFinished act

handleMessage :: StateT ResultSaverEnv BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                content = msg ^. Message.message
            processMessage topic content

processMessage :: String -> ByteString -> StateT ResultSaverEnv BusT ()
processMessage topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content

makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a, Show a) => (a -> StateT ResultSaverEnv BusT ()) -> (String, ByteString -> StateT ResultSaverEnv BusT ())
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h $ ((Bin.decode . fromStrict $ content) :: a)

handlersMap :: Map String (ByteString -> StateT ResultSaverEnv BusT ())
handlersMap = Map.fromList [ makeHandler importProjectResponseHandler
                           , makeHandler getProgramResponseHandler
                           , makeHandler nodesUpdateHandler
                           , makeHandler nodeResultUpdateHandler
                           ]

importProjectResponseHandler :: ImportProject.Response -> StateT ResultSaverEnv BusT ()
importProjectResponseHandler response = do
  let requestId = response ^. Response.requestId
  case response ^. Response.status of
    Response.Ok (ImportProject.Result projectId _) -> do
      state <- use Env.state
      case state of
        Env.ImportRequested reqId -> do
          if requestId /= reqId then return ()
                                else do
                                    uuid <- liftIO $ UUID.nextRandom
                                    let request = Request uuid $ GetProgram.Request $ GraphLocation projectId 0 (Breadcrumb [])
                                    sendToBus request
                                    Env.state .= Env.ProgramRequested uuid
        _ -> return ()
    Response.Error msg -> Env.state .= Env.Error msg

getProgramResponseHandler :: GetProgram.Response -> StateT ResultSaverEnv BusT ()
getProgramResponseHandler response = do
  let requestId = response ^. Response.requestId
  case response ^. Response.status of
    Response.Ok program -> do
      state <- use Env.state
      case state of
        Env.ProgramRequested reqId -> do
          if requestId /= reqId then return ()
                                else do
                                  Env.state .= (Env.ProgramReceived (ProjectDump (program ^. GetProgram.graph) mempty mempty))
        _ -> return ()
    Response.Error msg -> Env.state .= Env.Error msg

nodesUpdateHandler :: NodesUpdate.Update -> StateT ResultSaverEnv BusT ()
nodesUpdateHandler (NodesUpdate.Update gl nodes') = do
  state <- use Env.state
  case state of
      st@(Env.ProgramReceived (ProjectDump program nodes results)) -> do
        let nodesIds = map (^. Node.nodeId) nodes'
            nodesMap = Map.fromList $ zip nodesIds nodes'
        Env.state .= Env.ProgramReceived (ProjectDump program (Map.union nodesMap nodes) results)
        checkIfDone
      _ -> return ()

nodeResultUpdateHandler :: NodeResultUpdate.Update -> StateT ResultSaverEnv BusT ()
nodeResultUpdateHandler (NodeResultUpdate.Update gl nodeId result _) = do
  state <- use Env.state
  case state of
      st@(Env.ProgramReceived (ProjectDump program nodes results)) -> do
        Env.state .= Env.ProgramReceived (ProjectDump program nodes (Map.insert nodeId result results))
        checkIfDone
      _ -> return ()

checkIfDone :: StateT ResultSaverEnv BusT ()
checkIfDone = do
  state <- use Env.state
  case state of
      st@(Env.ProgramReceived (ProjectDump program nodes results)) -> do
        let graphNodes = program ^. Graph.nodes
            nodeCount  = length graphNodes
            nodeUpdateCount = Map.size nodes
            nodeResultCount = Map.size results
        when (nodeCount == nodeResultCount && nodeCount == nodeUpdateCount) $ do
          Env.state .= Env.Finished (ProjectDump program nodes results)
      _ -> return ()
