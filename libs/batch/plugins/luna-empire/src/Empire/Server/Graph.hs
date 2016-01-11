{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Graph where

import           Prologue

import qualified Data.Binary                 as Bin
import           Control.Monad.State         (StateT, get, put)
import           Flowbox.Bus.BusT            (BusT (..))
import qualified Empire.Env                  as Env
import           Empire.Env                  (Env)
import           Data.Map.Strict             (Map)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text

import qualified Flowbox.Bus.Data.Flag       as Flag
import qualified Flowbox.Bus.Data.Message    as Message
import qualified Flowbox.Bus.Bus             as Bus
import           Flowbox.Bus.BusT            (BusT (..))
import qualified Flowbox.Bus.BusT            as Bus

import qualified Empire.API.Data.Node        as Node
import           Empire.API.Data.Node        (Node)
import qualified Empire.API.Data.NodeMeta    as NodeMeta
import           Empire.API.Data.NodeMeta    (NodeMeta)
import           Empire.API.Data.Library     (LibraryId)
import           Empire.API.Data.Project     (ProjectId)
import qualified Empire.API.Topic            as Topic
import qualified Empire.API.Graph.AddNode    as AddNode
import qualified Empire.API.Graph.RemoveNode as RemoveNode
import qualified Empire.API.Graph.UpdateNodeMeta as UpdateNodeMeta
import qualified Empire.API.Data.NodeMeta    as NodeMeta
import qualified Empire.API.Response         as Response

import qualified Empire.Commands.Graph       as Graph
import           Empire.Data.AST             (AST)
import qualified Empire.Empire               as Empire
import           Empire.Empire               (Empire)

import           Flowbox.System.Log.Logger

logger :: LoggerIO
logger = getLoggerIO $moduleName


addNode :: ProjectId -> LibraryId -> Text -> NodeMeta -> Empire Node
addNode pid lid expr meta = do
    node <- Graph.addNode pid lid expr meta
    return node

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    logger info $ "Handling AddNodeRequest"
    let request = Bin.decode . fromStrict $ content :: AddNode.Request
    logger info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger info $ show currentEmpireEnv
    (nodeE, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ addNode
        (request ^. AddNode.projectId)
        (request ^. AddNode.libraryId)
        (Text.pack $ request ^. AddNode.expr)
        (request ^. AddNode.nodeMeta)
    Env.empireEnv .= newEmpireEnv
    case nodeE of
        Left err -> logger info $ "Error processing request: " ++ show err
        Right node -> do
            let update   = AddNode.Update  node
                response = Response.Update request update
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message "empire.graph.node.add.update" $ toStrict $ Bin.encode response
            return ()

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    logger info $ "Handling RemoveNodeRequest"
    let removeNodeRequest = Bin.decode . fromStrict $ content :: RemoveNode.Request
    logger info $ show removeNodeRequest

updateNodeMeta :: ByteString -> StateT Env BusT ()
updateNodeMeta content = do
    logger info $ "Handling UpdateNodeMeta"
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
    logger info $ show request
    let response = Response.Update request $ UpdateNodeMeta.Update $ request ^. UpdateNodeMeta.nodeMeta
    void $ lift $ BusT $ Bus.send Flag.Enable $ Message.Message "empire.graph.node.updateMeta.update" $ toStrict $ Bin.encode response
