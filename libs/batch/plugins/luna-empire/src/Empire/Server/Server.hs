module Empire.Server.Server where

import qualified Compress
import           Control.Arrow                 ((&&&))
import           Control.Concurrent.STM.TChan  (writeTChan)
import           Control.Lens                  (to, use, (.=), (^..), _Left)
import           Control.Monad.Catch           (handle, try)
import           Control.Monad.State           (StateT)
import           Control.Monad.STM             (atomically)
import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Bin
import           Data.ByteString.Lazy          (toStrict)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           GHC.Stack                     (renderStack, whoCreated)
import           Prelude                       ((++))
import           Prologue
import           System.Environment            (getEnv)
import           System.FilePath               (replaceFileName, (</>))

import qualified Empire.Commands.Graph         as Graph
import           Empire.Data.AST               (SomeASTException)
import           Empire.Empire                 (Empire, runEmpire)
import           Empire.Env                    (Env)
import qualified Empire.Env                    as Env
import           Empire.Utils                  (currentISO8601Time)
import qualified Luna.Package.Structure.Name   as Package
import qualified LunaStudio.API.Graph.Request  as G
import           LunaStudio.API.Request        (Request (..))
import qualified LunaStudio.API.Response       as Response
import           LunaStudio.API.Topic          (MessageTopic)
import qualified LunaStudio.API.Topic          as Topic
import           LunaStudio.Data.Diff          (Diff, diff)
import           LunaStudio.Data.Error         (Error, LunaError, errorContent)
import           LunaStudio.Data.Graph         (Graph (..))
import qualified LunaStudio.Data.Graph         as GraphAPI
import           LunaStudio.Data.GraphLocation (GraphLocation (..))
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified LunaStudio.Data.Node          as Node
import qualified System.Log.MLogger            as Logger
import qualified ZMQ.Bus.Config                as Config
import qualified ZMQ.Bus.Data.Message          as Message
import qualified ZMQ.Bus.EndPoint              as EP
import           ZMQ.Bus.Trans                 (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = do
    chan <- use Env.toBusChan
    liftIO $ atomically $ writeTChan chan
        $ Message.Message topic $ Compress.pack $ Bin.encode bin

sendToBus' :: (MessageTopic a, Binary a) => a -> StateT Env BusT ()
sendToBus' msg = sendToBus (Topic.topic msg) msg

replyFail :: forall a b c. Response.ResponseResult a b c
    => Logger.Logger -> Error LunaError -> Request a -> Response.Status b
    -> StateT Env BusT ()
replyFail logger err req inv = do
    time <- liftIO currentISO8601Time
    logger Logger.error $ time
        <> "\t:: " <> formatErrorMessage req (Text.unpack $ err ^. errorContent)
    sendToBus' $ Response.error req inv err

replyOk :: forall a b. Response.ResponseResult a b ()
    => Request a -> b -> StateT Env BusT ()
replyOk req inv = do
    time <- liftIO currentISO8601Time
    logger Logger.info $ time <> "\t:: sending ok for " <> Topic.topic req
    sendToBus' $ Response.ok req inv

replyResult :: forall a b c. (Response.ResponseResult a b c, Show c)
    => Request a -> b -> c -> StateT Env BusT ()
replyResult req inv res = do
    time <- liftIO currentISO8601Time
    logger Logger.info $ time <> "\t:: sending response for " <> Topic.topic req
    logger Logger.info $ time <> "\t:: " <> show res
    sendToBus' $ Response.result req inv res

errorMessage :: String
errorMessage = "error during processing request "

formatErrorMessage :: MessageTopic a => a -> String -> String
formatErrorMessage req msg = errorMessage <> (Topic.topic req) <> ": " <> msg

defaultLibraryPath = "Main.luna"

webGUIHack :: G.GraphRequest req => req -> IO req
webGUIHack req = do
    lunaroot <- liftIO $ getEnv Package.lunaRootEnv
    let path = lunaroot </> "projects" </> defaultLibraryPath
        realLocation = req ^. G.location
        realFile     = realLocation ^. GraphLocation.filePath
        hackedReq    = if null realFile
            then req & G.location . GraphLocation.filePath .~ path
            else req
    return hackedReq

prettyException :: Exception e => e -> IO String
prettyException e = do
    stack <- whoCreated e
    return $ displayException e ++ "\n" ++ renderStack stack

modifyGraph :: forall req inv res res'.
    ( Show req
    , G.GraphRequest req
    , Response.ResponseResult req inv res')
    => (req -> Empire inv) -> (req -> Empire res)
    -> (Request req -> inv -> res -> StateT Env BusT ()) -> Request req
    -> StateT Env BusT ()
modifyGraph inverse action success origReq@(Request uuid guiID request') = do
    logger Logger.info $ Topic.topic origReq <> ": " <> show request'
    request          <- liftIO $ webGUIHack request'
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    endPoints        <- use $ Env.config . to EP.clientFromConfig
    inv'             <- liftIO $ try
        $ runEmpire empireNotifEnv currentEmpireEnv $ inverse request
    case inv' of
        Left (exc :: SomeException) -> do
            err <- liftIO $ Graph.prepareLunaError exc
            replyFail logger err origReq (Response.Error err)
        Right (inv, _) -> do
            let invStatus = Response.Ok inv
            result <- liftIO $ try
                $ runEmpire empireNotifEnv currentEmpireEnv $ action request
            case result of
                Left  (exc :: SomeException) -> do
                    err <- liftIO $ Graph.prepareLunaError exc
                    replyFail logger err origReq invStatus
                Right (result, newEmpireEnv) -> do
                    Env.empireEnv .= newEmpireEnv
                    success origReq inv result

modifyGraphOk :: forall req inv res .
    ( Show req
    , Bin.Binary req
    , G.GraphRequest req
    , Response.ResponseResult req inv ()
    ) => (req -> Empire inv) -> (req -> Empire res) -> Request req
    -> StateT Env BusT ()
modifyGraphOk inverse action = modifyGraph inverse action reply where
    reply req inv _ = replyOk req inv

defInverse :: a -> Empire ()
defInverse = const $ pure ()

catchAllExceptions :: Empire a -> Empire (Either SomeException a)
catchAllExceptions act = try act

withDefaultResult' :: (GraphLocation -> Empire Graph) -> GraphLocation
    -> Empire a -> Empire Diff
withDefaultResult' getFinalGraph location action = do
    oldGraph <- (_Left %~ Graph.prepareGraphError)
        <$> catchAllExceptions (Graph.getGraphNoTC location)
    void action
    newGraph <- (_Left %~ Graph.prepareGraphError)
        <$> catchAllExceptions (getFinalGraph location)
    pure $ diff oldGraph newGraph

withDefaultResult :: GraphLocation -> Empire a -> Empire Diff
withDefaultResult = withDefaultResult' Graph.getGraphNoTC

withDefaultResultTC :: GraphLocation -> Empire a -> Empire Diff
withDefaultResultTC = withDefaultResult' Graph.getGraph
