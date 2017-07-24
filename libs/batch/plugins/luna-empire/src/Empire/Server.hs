{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

module Empire.Server where

import qualified Codec.Compression.GZip           as GZip
import           Control.Concurrent               (forkIO)
import           Control.Concurrent.STM           (STM)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan     (TChan, newTChan, readTChan, tryPeekTChan)
import           Control.Monad                    (forM_, forever)
import           Control.Monad.Catch              (try, catchAll)
import           Control.Monad.State              (StateT, evalStateT)
import           Control.Monad.STM                (atomically)
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy.Char8       (unpack)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Map.Strict                  as Map

import           System.FilePath                  ()
import           System.FilePath.Find             (always, extension, find, (==?))
import           System.FilePath.Glob             ()
import           System.FilePath.Manip            ()


import           Empire.Data.AST                      (SomeASTException)
import           Empire.Data.Graph                    (ClsGraph, Graph, ast)
import qualified Empire.Data.Graph                    as Graph
import           LunaStudio.API.AsyncUpdate           (AsyncUpdate (..))
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import qualified LunaStudio.API.Graph.SetNodesMeta    as SetNodesMeta
import qualified LunaStudio.API.Topic                 as Topic
import           LunaStudio.Data.GraphLocation        (GraphLocation)

import qualified Empire.Commands.AST              as AST
import qualified Empire.Commands.Graph            as Graph (openFile)
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Persistence      as Persistence
import qualified Empire.Commands.Typecheck        as Typecheck
import           Empire.Commands.Typecheck        (Scope (..))
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import qualified Empire.Handlers                  as Handlers
import qualified Empire.Server.Graph              as Graph
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
import           System.Remote.Monitoring
import           System.Environment               (getEnv)
import           System.Directory                 (canonicalizePath)
import           System.Mem                       (performGC)

import System.IO.Unsafe (unsafePerformIO)

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendStarted :: BusEndPoints -> IO ()
sendStarted endPoints = do
    putStrLn "======= SENDING MESSAGE STARTED ========="
    let content = toStrict . GZip.compress .  Bin.encode $ EmpireStarted.Status
    putStrLn "====== compressed content ===== "
    print content
    putStrLn "===== End ========"
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic EmpireStarted.Status) content

run :: BusEndPoints -> [Topic] -> Bool -> FilePath -> IO (Either Bus.Error ())
run endPoints topics formatted projectRoot = do
    sendStarted endPoints
    forkServer "localhost" 1234
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    scope            <- newEmptyMVar
    toBusChan        <- atomically newTChan
    fromEmpireChan   <- atomically newTChan
    tcReq            <- newEmptyMVar
    let env     = Env.make toBusChan fromEmpireChan tcReq scope projectRoot
    let commEnv = Empire.CommunicationEnv fromEmpireChan tcReq scope
    forkIO $ void $ Bus.runBus endPoints $ BusT.runBusT $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.runBus endPoints $ startToBusWorker toBusChan
    forkIO $ void $ Bus.runBus endPoints $ startTCWorker commEnv tcReq scope
    Bus.runBus endPoints $ do
        mapM_ Bus.subscribe topics
        BusT.runBusT $ evalStateT (runBus formatted projectRoot) env

runBus :: Bool -> FilePath ->  StateT Env BusT ()
runBus formatted projectRoot = do
    Env.formatted   .= formatted
    Env.projectRoot .= projectRoot
    createDefaultState
    forever handleMessage

prepareStdlib :: IO (Scope, Empire.SymbolMap, IO ())
prepareStdlib = do
    lunaroot       <- canonicalizePath =<< getEnv "LUNAROOT"
    (cleanup, std) <- Typecheck.createStdlib $ lunaroot ++ "/Std/"
    return (std, Typecheck.getSymbolMap std, cleanup)

startTCWorker :: Empire.CommunicationEnv -> MVar (GraphLocation, ClsGraph, Bool) -> MVar Empire.SymbolMap -> Bus ()
startTCWorker env reqs scopeVar = liftIO $ do
    (Scope std, symbolMap, cleanup) <- prepareStdlib
    putMVar scopeVar symbolMap
    pmState <- Graph.defaultPMState
    let interpreterEnv = Empire.InterpreterEnv def def def Nothing undefined cleanup def std
    void $ Empire.runEmpire env interpreterEnv $ forever $ do
        (loc, g, flush) <- liftIO $ takeMVar reqs
        when flush
            Typecheck.flushCache
        Empire.graph .= (g & Graph.clsAst . Graph.pmState .~ pmState)
        liftIO performGC
        catchAll (Typecheck.run loc) print

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
        Left err -> logger Logger.error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg = show (crlID ^. Message.messageID) <> ": " <> show senderID
                         <> " -> (last = " <> show lastFrame
                         <> ")\t:: " <> topic
                content = GZip.decompress . fromStrict $ msg ^. Message.message
            case Utils.lastPart '.' topic of
                "update"  -> handleUpdate        logMsg topic content
                "status"  -> handleStatus        logMsg topic content
                "request" -> handleRequest       logMsg topic content
                "debug"   -> handleDebug         logMsg topic content
                _         -> handleNotRecognized logMsg topic content

defaultHandler :: BSL.ByteString -> StateT Env BusT ()
defaultHandler content = do
    logger Logger.error $ "Not recognized request"
    logger Logger.info $ unpack content

handleRequest :: String -> String -> BSL.ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger Logger.info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

handleUpdate :: String -> String -> BSL.ByteString -> StateT Env BusT ()
handleUpdate logMsg topic content = do
    logger Logger.info logMsg
    let update = if topic == "empire.graph.node.updateMeta.update"
                      then Just (Bin.decode content :: SetNodesMeta.Update)
                      else Nothing
    forM_ update $ Graph.handleSetNodesMetaUpdate

handleStatus :: String -> String -> BSL.ByteString -> StateT Env BusT ()
handleStatus logMsg _ content = logger Logger.info logMsg

handleDebug :: String -> String -> BSL.ByteString -> StateT Env BusT ()
handleDebug logMsg _ content = do
    logger Logger.info logMsg
    currentEmpireEnv <- use Env.empireEnv
    formatted        <- use Env.formatted
    logger Logger.debug $ Utils.display formatted currentEmpireEnv

handleNotRecognized :: String -> String -> BSL.ByteString -> StateT Env BusT ()
handleNotRecognized logMsg _ content = do
    logger Logger.error logMsg
    logger Logger.error $ show content
