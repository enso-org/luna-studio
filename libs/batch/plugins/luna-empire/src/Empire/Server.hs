{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Empire.Server where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.STM           (STM)
import           Control.Concurrent.STM.TChan     (TChan, newTChan, readTChan, tryPeekTChan)
import           Control.Monad                    (forever)
import           Control.Monad.State              (StateT, evalStateT)
import           Control.Monad.STM                (atomically)
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (unpack)
import           Data.ByteString.Lazy             (toStrict)
import qualified Data.Map.Strict                  as Map

import           System.FilePath                  ()
import           System.FilePath.Find             (always, extension, find, (==?))
import           System.FilePath.Glob             ()
import           System.FilePath.Manip            ()


import qualified Empire.API.Control.EmpireStarted as EmpireStarted
import           Empire.API.Data.AsyncUpdate      (AsyncUpdate (..))
import           Empire.API.Data.GraphLocation    (GraphLocation)
import qualified Empire.API.Graph.NodesUpdate     as NodesUpdate
import qualified Empire.API.Topic                 as Topic
import           Empire.Data.Graph                (Graph, ast)

import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Persistence      as Persistence
import qualified Empire.Commands.Project          as Project
import qualified Empire.Commands.Typecheck        as Typecheck
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import qualified Empire.Handlers                  as Handlers
import qualified Empire.Server.Server             as Server
import qualified Empire.Utils                     as Utils
import           Prologue                         hiding (Text)
import qualified System.Log.MLogger               as Logger
import           ZMQ.Bus.Bus                      (Bus)
import qualified ZMQ.Bus.Bus                      as Bus
import qualified ZMQ.Bus.Config                   as Config
import qualified ZMQ.Bus.Data.Flag                as Flag
import           ZMQ.Bus.Data.Message             (Message)
import qualified ZMQ.Bus.Data.Message             as Message
import           ZMQ.Bus.Data.MessageFrame        (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic               (Topic)
import           ZMQ.Bus.EndPoint                 (BusEndPoints)
import           ZMQ.Bus.Trans                    (BusT (..))
import qualified ZMQ.Bus.Trans                    as BusT

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendStarted :: BusEndPoints -> IO ()
sendStarted endPoints = do
    let content = toStrict . Bin.encode $ EmpireStarted.Status
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic EmpireStarted.Status) content

run :: BusEndPoints -> [Topic] -> Bool -> FilePath -> IO (Either Bus.Error ())
run endPoints topics formatted projectRoot = do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    sendStarted endPoints
    toBusChan      <- atomically newTChan
    fromEmpireChan <- atomically newTChan
    tcChan         <- atomically newTChan
    let env     = Env.make toBusChan fromEmpireChan tcChan projectRoot
    let commEnv = Empire.CommunicationEnv fromEmpireChan tcChan
    forkIO $ void $ Bus.runBus endPoints $ BusT.runBusT $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.runBus endPoints $ startToBusWorker toBusChan
    forkIO $ void $ Bus.runBus endPoints $ startTCWorker commEnv tcChan
    Bus.runBus endPoints $ do
        mapM_ Bus.subscribe topics
        BusT.runBusT $ evalStateT (runBus formatted projectRoot) env

runBus :: Bool -> FilePath ->  StateT Env BusT ()
runBus formatted projectRoot = do
    Env.formatted   .= formatted
    Env.projectRoot .= projectRoot
    createDefaultState
    forever handleMessage

readAll :: TChan a -> STM a
readAll chan = do
    v    <- readTChan chan
    next <- tryPeekTChan chan
    case next of
        Nothing -> return v
        Just _  -> readAll chan

startTCWorker :: Empire.CommunicationEnv -> TChan (GraphLocation, Graph, Bool) -> Bus ()
startTCWorker env chan = liftIO $ void $ Empire.runEmpire env def $ forever $ do
    (loc, g, flush) <- liftIO $ atomically $ readAll chan
    if flush then Typecheck.flushCache else return ()
    Empire.graph .= g
    Typecheck.run loc
    {-zoom (Empire.graph . ast) $ AST.dumpGraphViz "tc_graph"-}


startToBusWorker :: TChan Message -> Bus ()
startToBusWorker toBusChan = forever $ do
    msg <- liftIO $ atomically $ readTChan toBusChan
    Bus.send Flag.Enable msg

startAsyncUpdateWorker :: TChan AsyncUpdate -> StateT Env BusT ()
startAsyncUpdateWorker asyncChan = forever $ do
    update <- liftIO $ atomically $ readTChan asyncChan
    case update of
        NodesUpdate      up -> Server.sendToBus' up
        ResultUpdate     up -> Server.sendToBus' up
        ConnectionUpdate up -> Server.sendToBus' up

projectFiles :: FilePath -> IO [FilePath]
projectFiles = find always (extension ==? ".lproj")

loadAllProjects :: StateT Env BusT ()
loadAllProjects = do
  projectRoot  <- use Env.projectRoot
  empireNotifEnv   <- use Env.empireNotif

  projects <- liftIO $ projectFiles projectRoot
  loadedProjects <- flip mapM projects $ \proj -> do
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.loadProject proj
    case result of
        Left err -> do
          logger Logger.error $ "Cannot load project [" <> proj <> "]: " <> err
          return Nothing
        Right projectId -> do
          Env.empireEnv .= newEmpireEnv
          return $ Just projectId

  when ((catMaybes loadedProjects) == []) $ do
    currentEmpireEnv <- use Env.empireEnv
    (_, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $  Persistence.createDefaultProject
    Env.empireEnv .= newEmpireEnv



createDefaultState :: StateT Env BusT ()
createDefaultState = loadAllProjects

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg = show (crlID ^. Message.messageID) <> ": " <> show senderID
                         <> " -> (last = " <> show lastFrame
                         <> ")\t:: " <> topic
                content = msg ^. Message.message
            case Utils.lastPart '.' topic of
                "update"  -> handleUpdate        logMsg topic content
                "status"  -> handleStatus        logMsg topic content
                "request" -> handleRequest       logMsg topic content
                "debug"   -> handleDebug         logMsg topic content
                _         -> handleNotRecognized logMsg topic content


defaultHandler :: ByteString -> StateT Env BusT ()
defaultHandler content = do
    logger Logger.error $ "Not recognized request"
    logger Logger.info $ unpack content

handleRequest :: String -> String -> ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger Logger.info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

handleUpdate :: String -> String -> ByteString -> StateT Env BusT ()
handleUpdate logMsg _ content = do
    logger Logger.info logMsg

handleStatus :: String -> String -> ByteString -> StateT Env BusT ()
handleStatus logMsg _ content = do
    logger Logger.info logMsg

handleDebug :: String -> String -> ByteString -> StateT Env BusT ()
handleDebug logMsg _ content = do
    logger Logger.info logMsg
    currentEmpireEnv <- use Env.empireEnv
    formatted        <- use Env.formatted
    logger Logger.debug $ Utils.display formatted currentEmpireEnv

handleNotRecognized :: String -> String -> ByteString -> StateT Env BusT ()
handleNotRecognized logMsg _ content = do
    logger Logger.error logMsg
    logger Logger.error $ show content
