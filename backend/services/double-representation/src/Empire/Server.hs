{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Server where

import Prologue hiding (Text)

import qualified Bus.Data.Message                     as Message
import qualified Bus.Framework.App                    as Bus
import qualified Compress
import qualified Control.Concurrent.Async             as Async
import qualified Control.Concurrent.Async.Lifted      as AsyncL
import qualified Control.Exception.Safe               as Exception
import qualified Data.Bimap                           as Bimap
import qualified Data.Binary                          as Bin
import qualified Data.Map.Strict                      as Map
import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Graph                as Graph
import qualified Empire.Commands.Library              as Library
import qualified Empire.Commands.Persistence          as Persistence
import qualified Empire.Commands.Typecheck            as Typecheck
import qualified Empire.Data.Graph                    as Graph
import qualified Empire.Empire                        as Empire
import qualified Empire.Env                           as Env
import qualified Empire.Handlers                      as Handlers
import qualified Empire.Server.Graph                  as Graph
import qualified Empire.Server.Server                 as Server
import qualified Empire.Utils                         as Utils
import qualified Luna.Package                         as Package
import qualified Luna.Pass.Flow.ProcessUnits          as ProcessUnits
import qualified Luna.Pass.Resolve.Data.Resolution    as Res
import qualified Luna.Pass.Scheduler                  as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit         as Unit
import qualified Luna.Pass.Sourcing.UnitLoader        as UnitLoader
import qualified Luna.Pass.Sourcing.UnitMapper        as UnitMap
import qualified Luna.Std                             as Std
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import qualified LunaStudio.API.Graph.SetNodesMeta    as SetNodesMeta
import qualified LunaStudio.API.Topic                 as Topic
import qualified Path
import qualified System.Log.MLogger                   as Logger

import Bus.Data.Config               (Config)
import Bus.Data.Message              (Message, Topic)
import Control.Concurrent            (forkIO, forkOn)
import Control.Concurrent.Async      (Async)
import Control.Concurrent.MVar
import Control.Concurrent.STM        (STM)
import Control.Concurrent.STM.TChan  (TChan, newTChan, readTChan, tryPeekTChan)
import Control.Lens                  (use, (.=))
import Control.Monad                 (forever)
import Control.Monad.State           (StateT, evalStateT)
import Control.Monad.STM             (atomically)
import Data.ByteString.Lazy          (ByteString)
import Data.ByteString.Lazy.Char8    (unpack)
import Data.IORef
import Empire.ASTOp                  (liftScheduler)
import Empire.Data.AST               (SomeASTException)
import Empire.Data.Graph             (ClsGraph, Graph)
import Empire.Env                    (Env)
import Luna.Pass.Data.Stage          (Stage)
import LunaStudio.API.AsyncUpdate    (AsyncUpdate (..))
import LunaStudio.Data.GraphLocation (GraphLocation)
import System.Directory              (canonicalizePath)
import System.Environment            (getEnv)
import System.FilePath               ()
import System.FilePath.Find          (always, extension, find, (==?))
import System.FilePath.Glob          ()
import System.FilePath.Manip         ()
import System.IO.Unsafe              (unsafePerformIO)
import System.Mem                    (performGC)
import System.Remote.Monitoring

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendStarted :: Config -> IO ()
sendStarted endPoints = do
    let content = Compress.pack .  Bin.encode $ EmpireStarted.Status
    void $ Bus.run endPoints $ Bus.send (Topic.topic' EmpireStarted.Status) content

requestCapability, tcCapability :: Int
requestCapability = 0
tcCapability      = 1

run :: Config -> [Topic] -> IO ()
run endPoints topics = do
    forkServer "localhost" 1234
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display True) endPoints
    toBusChan        <- atomically newTChan
    fromEmpireChan   <- atomically newTChan
    tcReq            <- newEmptyMVar
    env              <- Env.make toBusChan fromEmpireChan tcReq
    let commEnv = env ^. Env.empireNotif
    forkIO $ void $ Bus.run endPoints $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.run endPoints $ startToBusWorker toBusChan
    waiting <- newEmptyMVar
    requestThread <- forkOn requestCapability $ void $ Bus.run endPoints $ do
        Bus.subscribe topics
        evalStateT runBus env
        liftIO $ putMVar waiting ()
    compiledStdlib <- newEmptyMVar
    forkOn tcCapability $ void $ Bus.run endPoints $ startTCWorker commEnv
    sendStarted endPoints
    takeMVar waiting

runBus :: StateT Env Bus.App ()
runBus = forever handleMessage

startTCWorker :: Empire.CommunicationEnv -> Bus.App ()
startTCWorker env = liftIO $ do
    let reqs = env ^. Empire.typecheckChan
    pmState <- Graph.defaultPMState
    let commandState   = Graph.CommandState pmState Nothing
    void $ Empire.evalEmpire env commandState $ do
        Empire.zoomCommandMaybe $ Typecheck.makePrimStdIfMissing
        forever $ do
            req <- liftIO $ takeMVar reqs
            case req of
                Empire.TCRun (Empire.TCRunRequest loc g rooted interpret recompute) -> do
                    as <- AsyncL.asyncOn tcCapability $ do
                        Typecheck.runIfActive loc g rooted interpret recompute `Exception.onException`
                            Typecheck.stop
                    res <- AsyncL.waitCatch as
                    case res of
                        Left exc -> logger Logger.warning $ "TCWorker: TC failed with: " <> displayException exc
                        Right _  -> return ()
                Empire.TCStop -> Typecheck.stop
                Empire.TCSetProject root -> Typecheck.setProject root

startToBusWorker :: TChan Message -> Bus.App ()
startToBusWorker toBusChan = forever $ do
    msg <- liftIO $ atomically $ readTChan toBusChan
    Bus.sendMessage msg

startAsyncUpdateWorker :: TChan AsyncUpdate -> StateT Env Bus.App ()
startAsyncUpdateWorker asyncChan = forever $ do
    update <- liftIO $ atomically $ readTChan asyncChan
    case update of
        MonadsUpdate      up -> Server.sendToBus' up
        TypecheckerUpdate up -> Server.sendToBus' up
        ResultUpdate      up -> Server.sendToBus' up
        CodeUpdate        up -> Server.sendToBus' up
        InterpreterUpdate up -> Server.sendToBus' up

handleMessage :: StateT Env Bus.App ()
handleMessage = do
    msg <- lift Bus.receive
    let handler :: MonadIO m => SomeException -> m ()
        handler e = do
            excMsg <- liftIO $ Graph.prettyException e
            logger Logger.error $ "Uncaught exception: " <> excMsg
    Exception.handle handler $ do
        time <- liftIO Utils.currentISO8601Time
        let topic   = msg ^. Message.topic
            logMsg  = time <> "\t:: received " <> topic
            content = Compress.unpack $ msg ^. Message.body
        case Utils.lastPart '.' topic of
            "update"    -> handleUpdate        logMsg topic content
            "status"    -> handleStatus        logMsg topic content
            "request"   -> handleRequest       logMsg topic content
            "debug"     -> handleDebug         logMsg topic content
            "response"  -> handleResponse      logMsg topic content
            "typecheck" -> handleTypecheck     logMsg topic content
            _           -> handleNotRecognized logMsg topic content

defaultHandler :: ByteString -> StateT Env Bus.App ()
defaultHandler content = do
    logger Logger.error $ "Not recognized request"

handleRequest :: String -> String -> ByteString -> StateT Env Bus.App ()
handleRequest logMsg topic content = do
    logger Logger.info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

handleUpdate :: String -> String -> ByteString -> StateT Env Bus.App ()
handleUpdate logMsg topic content = do
    logger Logger.info logMsg

handleStatus :: String -> String -> ByteString -> StateT Env Bus.App ()
handleStatus logMsg _ content = logger Logger.info logMsg

handleDebug :: String -> String -> ByteString -> StateT Env Bus.App ()
handleDebug logMsg _ content = do
    logger Logger.info logMsg
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.debug $ Utils.display True currentEmpireEnv

handleNotRecognized :: String -> String -> ByteString -> StateT Env Bus.App ()
handleNotRecognized logMsg _ content = do
    logger Logger.error logMsg
    logger Logger.error $ show content

handleResponse :: String -> String -> ByteString -> StateT Env Bus.App ()
handleResponse logMsg _ content = do
    logger Logger.info logMsg

handleTypecheck :: String -> String -> ByteString -> StateT Env Bus.App ()
handleTypecheck logMsg _ content = do
    logger Logger.info logMsg
