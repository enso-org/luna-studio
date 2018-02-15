{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Server where

import qualified Compress
import           Control.Concurrent                   (forkIO, forkOn)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM               (STM)
import           Control.Concurrent.STM.TChan         (TChan, newTChan, readTChan, tryPeekTChan)
import           Control.Monad                        (forM_, forever)
import           Control.Monad.Catch                  (catchAll, try)
import           Control.Monad.State                  (StateT, evalStateT)
import           Control.Monad.STM                    (atomically)
import qualified Data.Binary                          as Bin
import           Data.ByteString.Lazy                 (ByteString)
import           Data.ByteString.Lazy.Char8           (unpack)
import           Data.IORef
import qualified Data.Map.Strict                      as Map

import           System.FilePath                      ()
import           System.FilePath.Find                 (always, extension, find, (==?))
import           System.FilePath.Glob                 ()
import           System.FilePath.Manip                ()

import           Data.Future                          (minCapabilityNumber, updateCapabilities)
import           Empire.Data.AST                      (SomeASTException)
import           Empire.Data.Graph                    (ClsGraph, Graph, ast)
import qualified Empire.Data.Graph                    as Graph
import           LunaStudio.API.AsyncUpdate           (AsyncUpdate (..))
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import qualified LunaStudio.API.Graph.SetNodesMeta    as SetNodesMeta
import qualified LunaStudio.API.Topic                 as Topic
import           LunaStudio.Data.GraphLocation        (GraphLocation)

import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Graph                as Graph (openFile)
import qualified Empire.Commands.Library              as Library
import qualified Empire.Commands.Persistence          as Persistence
import           Empire.Commands.Typecheck            (Scope (..))
import qualified Empire.Commands.Typecheck            as Typecheck
import qualified Empire.Empire                        as Empire
import           Empire.Env                           (Env)
import qualified Empire.Env                           as Env
import qualified Empire.Handlers                      as Handlers
import qualified Empire.Server.Graph                  as Graph
import qualified Empire.Server.Server                 as Server
import qualified Empire.Utils                         as Utils
import           Prologue                             hiding (Text)
import           System.Directory                     (canonicalizePath)
import           System.Environment                   (getEnv)
import qualified System.Log.MLogger                   as Logger
import           System.Mem                           (performGC)

import           System.Remote.Monitoring
import           ZMQ.Bus.Bus                          (Bus)
import qualified ZMQ.Bus.Bus                          as Bus
import qualified ZMQ.Bus.Data.Flag                    as Flag
import           ZMQ.Bus.Data.Message                 (Message)
import qualified ZMQ.Bus.Data.Message                 as Message
import           ZMQ.Bus.Data.MessageFrame            (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic                   (Topic)
import           ZMQ.Bus.EndPoint                     (BusEndPoints)
import           ZMQ.Bus.Trans                        (BusT (..))
import qualified ZMQ.Bus.Trans                        as BusT

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendStarted :: BusEndPoints -> IO ()
sendStarted endPoints = do
    let content = Compress.pack .  Bin.encode $ EmpireStarted.Status
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic EmpireStarted.Status) content

requestCapability, tcCapability :: Int
requestCapability = 0
tcCapability      = 1

run :: BusEndPoints -> [Topic] -> Bool -> FilePath -> IO ()
run endPoints topics formatted projectRoot = do
    sendStarted endPoints
    forkServer "localhost" 1234
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    scope            <- newEmptyMVar
    toBusChan        <- atomically newTChan
    fromEmpireChan   <- atomically newTChan
    tcReq            <- newEmptyMVar
    modules          <- newEmptyMVar
    env              <- Env.make toBusChan fromEmpireChan tcReq scope modules projectRoot
    let commEnv = env ^. Env.empireNotif
    forkIO $ void $ Bus.runBus endPoints $ BusT.runBusT $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.runBus endPoints $ startToBusWorker toBusChan
    forkOn tcCapability $ void $ Bus.runBus endPoints $ startTCWorker commEnv
    waiting <- newEmptyMVar
    requestThread <- forkOn requestCapability $ void $ Bus.runBus endPoints $ do
        mapM_ Bus.subscribe topics
        BusT.runBusT $ evalStateT (runBus formatted projectRoot) env
        liftIO $ putMVar waiting ()
    takeMVar waiting

runBus :: Bool -> FilePath ->  StateT Env BusT ()
runBus formatted projectRoot = do
    Env.formatted   .= formatted
    Env.projectRoot .= projectRoot
    createDefaultState
    forever handleMessage

prepareStdlib :: IO (Scope, IO ())
prepareStdlib = do
    lunaroot       <- canonicalizePath =<< getEnv "LUNAROOT"
    (cleanup, std) <- Typecheck.createStdlib $ lunaroot <> "/Std/"
    return (std, cleanup)

startTCWorker :: Empire.CommunicationEnv -> Bus ()
startTCWorker env = liftIO $ do
    let reqs = env ^. Empire.typecheckChan
        modules = env ^. Empire.modules
    writeIORef minCapabilityNumber 1
    updateCapabilities
    (std, cleanup) <- prepareStdlib
    putMVar modules $ unwrap std
    pmState <- Graph.defaultPMState
    let interpreterEnv = Empire.InterpreterEnv def def def undefined cleanup def
    void $ Empire.runEmpire env interpreterEnv $ forever $ do
        Empire.TCRequest loc g flush interpret recompute stop <- liftIO $ takeMVar reqs
        case stop of
            True  -> Typecheck.stop
            False -> do
                when flush
                    Typecheck.flushCache
                Empire.graph .= (g & Graph.clsAst . Graph.pmState .~ pmState)
                liftIO performGC
                catchAll (Typecheck.run modules loc interpret recompute) print

startToBusWorker :: TChan Message -> Bus ()
startToBusWorker toBusChan = forever $ do
    msg <- liftIO $ atomically $ readTChan toBusChan
    Bus.send Flag.Enable msg

startAsyncUpdateWorker :: TChan AsyncUpdate -> StateT Env BusT ()
startAsyncUpdateWorker asyncChan = forever $ do
    update <- liftIO $ atomically $ readTChan asyncChan
    case update of
        MonadsUpdate      up -> Server.sendToBus' up
        TypecheckerUpdate up -> Server.sendToBus' up
        ResultUpdate      up -> Server.sendToBus' up
        CodeUpdate        up -> Server.sendToBus' up
        InterpreterUpdate up -> Server.sendToBus' up

projectFiles :: FilePath -> IO [FilePath]
projectFiles = find always (extension ==? ".luna")

loadAllProjects :: StateT Env BusT ()
loadAllProjects = do
  projectRoot  <- use Env.projectRoot
  empireNotifEnv   <- use Env.empireNotif

  projects <- liftIO $ projectFiles projectRoot
  loadedProjects <- flip mapM projects $ \proj -> do
    currentEmpireEnv <- use Env.empireEnv
    result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.openFile proj
    case result of
        Left (exc :: SomeASTException) -> do
          logger Logger.error $ "Cannot load project [" <> proj <> "]: " <> (displayException exc)
          return Nothing
        Right (_, newEmpireEnv) -> do
          Env.empireEnv .= newEmpireEnv
          return $ Just ()

  when ((catMaybes loadedProjects) == []) $ do
    currentEmpireEnv <- use Env.empireEnv
    result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $  Persistence.createDefaultProject
    case result of
        Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
        Left (exc :: SomeASTException) -> return ()



createDefaultState :: StateT Env BusT ()
createDefaultState = loadAllProjects

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            time <- liftIO Utils.currentISO8601Time
            let topic   = msg ^. Message.topic
                logMsg  = time <> "\t:: received " <> topic
                content = Compress.unpack $ msg ^. Message.message
            case Utils.lastPart '.' topic of
                "update"    -> handleUpdate        logMsg topic content
                "status"    -> handleStatus        logMsg topic content
                "request"   -> handleRequest       logMsg topic content
                "debug"     -> handleDebug         logMsg topic content
                "response"  -> handleResponse      logMsg topic content
                "typecheck" -> handleTypecheck     logMsg topic content
                _           -> handleNotRecognized logMsg topic content

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
handleUpdate logMsg topic content = do
    logger Logger.info logMsg
    let update = if topic == "empire.graph.node.updateMeta.update"
                      then Just (Bin.decode content :: SetNodesMeta.Update)
                      else Nothing
    forM_ update $ Graph.handleSetNodesMetaUpdate

handleStatus :: String -> String -> ByteString -> StateT Env BusT ()
handleStatus logMsg _ content = logger Logger.info logMsg

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

handleResponse :: String -> String -> ByteString -> StateT Env BusT ()
handleResponse logMsg _ content = do
    logger Logger.info logMsg

handleTypecheck :: String -> String -> ByteString -> StateT Env BusT ()
handleTypecheck logMsg _ content = do
    logger Logger.info logMsg
