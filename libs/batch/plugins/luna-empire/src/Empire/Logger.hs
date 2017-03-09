{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Logger where

import           Control.Monad                     (forever)
import           Control.Monad.State               (StateT, evalStateT)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (unpack)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Prologue

import qualified Empire.API.Control.EmpireStarted  as EmpireStarted
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.CodeUpdate       as CodeUpdate
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.DumpGraphViz     as DumpGraphViz
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Graph.NodesUpdate      as NodesUpdate
import qualified Empire.API.Graph.RemoveConnection as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes      as RemoveNodes
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.TypeCheck        as TypeCheck
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ExportProject  as ExportProject
import qualified Empire.API.Project.ImportProject  as ImportProject
import qualified Empire.API.Project.ListProjects   as ListProjects
import qualified Empire.API.Project.OpenProject    as OpenProject
import           Empire.API.Request                (Request)
import qualified Empire.API.Topic                  as Topic
import qualified Empire.Commands.Library           as Library
import qualified Empire.Commands.Project           as Project
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (LoggerEnv)
import qualified Empire.Env                        as Env
import qualified Empire.Handlers                   as Handlers
import qualified Empire.Utils                      as Utils

import qualified System.Log.MLogger                as Logger
import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic                (Topic)
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import           ZMQ.Bus.Trans                     (BusT (..))
import qualified ZMQ.Bus.Trans                     as Bus


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

run :: BusEndPoints -> [Topic] -> Bool -> IO (Either Bus.Error ())
run endPoints topics formatted = Bus.runBus endPoints $ do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runBus formatted) def

runBus :: Bool -> StateT LoggerEnv BusT ()
runBus formatted = do
    Env.formatLog .= formatted
    forever handleMessage

handleMessage :: StateT LoggerEnv BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg = show (crlID ^. Message.messageID) <> ": " <> show senderID
                         <> " -> (last = " <> show lastFrame
                         <> ")\t:: " <> topic
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "request"  -> logMessage logMsg topic content
                "response" -> logMessage logMsg topic content
                "update"   -> logMessage logMsg topic content
                "debug"    -> logMessage logMsg topic content
                _          -> do logger Logger.error logMsg
                                 logger Logger.error errorMsg

type LogFormatter = (forall a. Show a => a -> String) -> ByteString -> String

-- How existentials should be used:
-- newtype Ex1 = forall a. Show a => Ex1 a
-- type LogFormatter = (Ex1 -> String) -> ByteString -> String

logMessage :: String -> String -> ByteString -> StateT LoggerEnv BusT ()
logMessage logMsg topic content = do
    return ()
--     formatted <- use Env.formatLog
--     logger Logger.info logMsg
--     let logFormatter = Map.findWithDefault defaultLogFormatter topic loggFormattersMap :: LogFormatter
--     logger Logger.debug $ logFormatter (Utils.display formatted) content

-- TODO: Fix this
-- makeHandler :: (Topic.MessageTopic a, Bin.Binary a, Show a) => Proxy a -> (String, LogFormatter)
-- makeHandler h = (Topic.topic h, process) where
--    process display content = display request where request = ((Bin.decode . fromStrict $ content) :: a)

-- loggFormattersMap :: Map String LogFormatter
-- loggFormattersMap = Map.fromList
--     [ makeHandler (Proxy :: Proxy (Request AddNode.Request          ))
--     , makeHandler (Proxy :: Proxy (AddNode.Response         ))
--     , makeHandler (Proxy :: Proxy (Request RemoveNodes.Request       ))
--     , makeHandler (Proxy :: Proxy (RemoveNodes.Response      ))
--     , makeHandler (Proxy :: Proxy (Request UpdateNodeMeta.Request   ))
--     , makeHandler (Proxy :: Proxy (UpdateNodeMeta.Response  ))
--     , makeHandler (Proxy :: Proxy (UpdateNodeMeta.Update    ))
--     , makeHandler (Proxy :: Proxy (Request RenameNode.Request       ))
--     , makeHandler (Proxy :: Proxy (RenameNode.Response      ))
--     , makeHandler (Proxy :: Proxy (RenameNode.Update        ))
--     , makeHandler (Proxy :: Proxy (Request Connect.Request          ))
--     , makeHandler (Proxy :: Proxy (Connect.Response         ))
--     , makeHandler (Proxy :: Proxy (Connect.Update           ))
--     , makeHandler (Proxy :: Proxy (Request RemoveConnection.Request       ))
--     , makeHandler (Proxy :: Proxy (RemoveConnection.Response      ))
--     , makeHandler (Proxy :: Proxy (RemoveConnection.Update        ))
--     , makeHandler (Proxy :: Proxy (Request GetProgram.Request       ))
--     , makeHandler (Proxy :: Proxy (GetProgram.Response      ))
--     , makeHandler (Proxy :: Proxy (NodesUpdate.Update        ))
--     , makeHandler (Proxy :: Proxy (NodeResultUpdate.Update  ))
--     , makeHandler (Proxy :: Proxy (CodeUpdate.Update        ))
--     , makeHandler (Proxy :: Proxy (Request CreateProject.Request    ))
--     , makeHandler (Proxy :: Proxy (CreateProject.Response   ))
--     , makeHandler (Proxy :: Proxy (CreateProject.Update     ))
--     , makeHandler (Proxy :: Proxy (Request ListProjects.Request     ))
--     , makeHandler (Proxy :: Proxy (ListProjects.Response    ))
--     , makeHandler (Proxy :: Proxy (ListProjects.Update    ))
--     , makeHandler (Proxy :: Proxy (Request ExportProject.Request     ))
--     , makeHandler (Proxy :: Proxy (ExportProject.Response    ))
--     , makeHandler (Proxy :: Proxy (Request ImportProject.Request     ))
--     , makeHandler (Proxy :: Proxy (ImportProject.Response    ))
--     , makeHandler (Proxy :: Proxy (Request CreateLibrary.Request    ))
--     , makeHandler (Proxy :: Proxy (CreateLibrary.Response   ))
--     , makeHandler (Proxy :: Proxy (CreateLibrary.Update     ))
--     , makeHandler (Proxy :: Proxy (Request ListLibraries.Request    ))
--     , makeHandler (Proxy :: Proxy (ListLibraries.Response   ))
--     , makeHandler (Proxy :: Proxy (Request SetDefaultValue.Request  ))
--     , makeHandler (Proxy :: Proxy (SetDefaultValue.Response ))
--     , makeHandler (Proxy :: Proxy (EmpireStarted.Status     ))
--     , makeHandler (Proxy :: Proxy (Request DumpGraphViz.Request     ))
--     , makeHandler (Proxy :: Proxy (Request TypeCheck.Request       ))
--     , makeHandler (Proxy :: Proxy (TypeCheck.Response       ))
--     ]

defaultLogFormatter :: LogFormatter
defaultLogFormatter = \display _ -> "Not recognized message"
