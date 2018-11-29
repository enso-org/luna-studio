{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Server where

import qualified Compress
import           Control.Concurrent                   (forkIO, forkOn)
import           Control.Concurrent.Async             (Async)
import qualified Control.Concurrent.Async             as Async
import qualified Control.Concurrent.Async.Lifted      as AsyncL
import           Control.Concurrent.MVar
import           Control.Concurrent.STM               (STM)
import           Control.Concurrent.STM.TChan         (TChan, newTChan, readTChan, tryPeekTChan)
import qualified Control.Exception.Safe               as Exception
import           Control.Lens                         ((.=), use)
import           Control.Monad                        (forever)
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

-- import           Data.Future                          (minCapabilityNumber, updateCapabilities)
import           Empire.Data.AST                      (SomeASTException)
import           Empire.Data.Graph                    (ClsGraph, Graph)
import qualified Empire.Data.Graph                    as Graph
import qualified Luna.Package                         as Package
import           LunaStudio.API.AsyncUpdate           (AsyncUpdate (..))
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import qualified LunaStudio.API.Graph.SetNodesMeta    as SetNodesMeta
import qualified LunaStudio.API.Topic                 as Topic
import           LunaStudio.Data.GraphLocation        (GraphLocation)

import           Empire.ASTOp                         (liftScheduler)
import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Graph                as Graph
import qualified Empire.Commands.Library              as Library
import qualified Empire.Commands.Persistence          as Persistence
-- import           Empire.Commands.Typecheck            (Scope (..))
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
import           System.IO.Unsafe                     (unsafePerformIO)
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

import qualified Luna.Pass.Sourcing.UnitLoader as UnitLoader
import qualified Luna.Pass.Sourcing.Data.Unit  as Unit
import           Luna.Pass.Data.Stage (Stage)
import qualified Luna.Pass.Resolve.Data.Resolution  as Res
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Pass.Sourcing.UnitMapper as UnitMap
import qualified Luna.Std as Std
import qualified Data.Bimap as Bimap
import qualified Path
import qualified Luna.Pass.Flow.ProcessUnits as ProcessUnits

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendStarted :: BusEndPoints -> IO ()
sendStarted endPoints = do
    let content = Compress.pack .  Bin.encode $ EmpireStarted.Status
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message (Topic.topic' EmpireStarted.Status) content

requestCapability, tcCapability :: Int
requestCapability = 0
tcCapability      = 1

run :: BusEndPoints -> [Topic] -> Bool -> FilePath -> IO ()
run endPoints topics formatted packageRoot = do
    forkServer "localhost" 1234
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    toBusChan        <- atomically newTChan
    fromEmpireChan   <- atomically newTChan
    tcReq            <- newEmptyMVar
    modules          <- newEmptyMVar
    env              <- Env.make toBusChan fromEmpireChan tcReq modules packageRoot
    let commEnv = env ^. Env.empireNotif
    forkIO $ void $ Bus.runBus endPoints $ BusT.runBusT $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.runBus endPoints $ startToBusWorker toBusChan
    waiting <- newEmptyMVar
    requestThread <- forkOn requestCapability $ void $ Bus.runBus endPoints $ do
        mapM_ Bus.subscribe topics
        BusT.runBusT $ evalStateT (runBus formatted packageRoot) env
        liftIO $ putMVar waiting ()
    compiledStdlib <- newEmptyMVar
    forkOn tcCapability $ void $ Bus.runBus endPoints $ startTCWorker commEnv
    sendStarted endPoints
    takeMVar waiting

runBus :: Bool -> FilePath ->  StateT Env BusT ()
runBus formatted projectRoot = do
    Env.formatted   .= formatted
    Env.projectRoot .= projectRoot
    createDefaultState
    forever handleMessage

-- killPreviousTC :: Empire.CommunicationEnv -> Maybe (Async Empire.InterpreterEnv) -> IO ()
-- killPreviousTC env prevAsync = case prevAsync of
--     Just a -> Async.poll a >>= \case
--         Just finished -> case finished of
--             Left exc     -> logger Logger.warning $ "[TCWorker]: TC failed with: " <> displayException exc
--             Right intEnv -> do
--                 logger Logger.info "[TCWorker]: killing listeners"
--                 void $ Empire.evalEmpire env intEnv Typecheck.stop
--         _      -> do
--             logger Logger.info "[TCWorker]: cancelling previous request"
--             Async.uninterruptibleCancel a
--     _      -> return ()

startTCWorker :: Empire.CommunicationEnv -> Bus ()
startTCWorker env = liftIO $ do
    let reqs = env ^. Empire.typecheckChan
    pmState <- Graph.defaultPMState
    let interpreterEnv = Empire.InterpreterEnv
                            (pure ())
                            (error "startTCWorker: clsGraph")
                            mempty
                            def
                            def
                            def
                            def
        commandState   = Graph.CommandState pmState interpreterEnv
    void $ Empire.evalEmpire env commandState $ do
        Typecheck.makePrimStdIfMissing
        forever $ do
            Empire.TCRequest loc g rooted flush interpret recompute stop <- liftIO $ takeMVar reqs
            if stop then do
                Typecheck.stop
            else do
                as <- AsyncL.asyncOn tcCapability $ do
                    Typecheck.run loc g rooted interpret recompute `Exception.onException`
                        Typecheck.stop
                res <- AsyncL.waitCatch as
                case res of
                    Left exc -> logger Logger.warning $ "TCWorker: TC failed with: " <> displayException exc
                    Right _  -> return ()

--     tcAsync <- newEmptyMVar
--         modules = env ^. Empire.modules
--     (std, cleanup, pmState) <- readMVar compiledStdlib
--     putMVar modules $ unwrap std
--         prevAsync <- tryTakeMVar tcAsync
--         killPreviousTC env prevAsync
--         async     <- Async.asyncOn tcCapability (Empire.evalEmpire env interpreterEnv $ do
--             case stop of
--                 True  -> Typecheck.stop
--                 False -> do
--                     when flush
--                         Typecheck.flushCache
--                     Empire.graph .= (g & Graph.clsAst . Graph.pmState .~ pmState)
--                     liftIO performGC
--         when recompute $ void (Async.waitCatch async)
--         putMVar tcAsync async

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

packageFiles :: FilePath -> IO [FilePath]
packageFiles = find always (extension ==? ".luna")

loadAllPackages :: StateT Env BusT ()
loadAllPackages = do
    packageRoot  <- use Env.projectRoot
    empireNotifEnv   <- use Env.empireNotif
  
    packages <- liftIO $ packageFiles packageRoot
    loadedPackages <- flip mapM packages $ \proj -> do
        currentEmpireEnv <- use Env.empireEnv
        result <- liftIO $ Exception.try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.openFile proj
        case result of
            Left (exc :: SomeASTException) -> do
              logger Logger.error $ "Cannot load package [" <> proj <> "]: " <> (displayException exc)
              return Nothing
            Right (_, newEmpireEnv) -> do
              Env.empireEnv .= newEmpireEnv
              return $ Just ()
    return ()

  -- when ((catMaybes loadedPackages) == []) $ do
  --   currentEmpireEnv <- use Env.empireEnv
  --   result <- liftIO $ Exception.try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $  Persistence.createDefaultPackage
  --   case result of
  --       Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
  --       Left (exc :: SomeASTException) -> return ()



createDefaultState :: StateT Env BusT ()
createDefaultState = loadAllPackages

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let handler :: MonadIO m => SomeException -> m ()
                handler e = do
                    excMsg <- liftIO $ Graph.prettyException e
                    logger Logger.error $ "Uncaught exception: " <> excMsg
            Exception.handle handler $ do
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

handleRequest :: String -> String -> ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger Logger.info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

handleUpdate :: String -> String -> ByteString -> StateT Env BusT ()
handleUpdate logMsg topic content = do
    logger Logger.info logMsg

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
