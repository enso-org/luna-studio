module Empire.Commands.Publisher where

import           Control.Concurrent.STM.TChan           (writeTChan)
import           Control.Monad.Reader
import           Control.Monad.STM                      (atomically)
import           Data.Text                              (Text)
import           Empire.API.Data.AsyncUpdate            (AsyncUpdate (..))
import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.Node                   (ExpressionNode, NodeId, NodeTypecheckerUpdate)
import           Empire.API.Data.TypeRep                (TypeRep)
import           Empire.Data.Graph                      (Graph)
import           Empire.Empire
import           Empire.Prelude

import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Graph.MonadsUpdate          as Monads
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified Empire.API.Graph.NodesUpdate           as Node
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

notifyMonadsUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> [MonadPath] -> m ()
notifyMonadsUpdate loc m =
    sendUpdate $ MonadsUpdate $ Monads.Update loc m

notifyNodeUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> ExpressionNode -> m ()
notifyNodeUpdate loc n =
    sendUpdate $ NodesUpdate $ Node.Update loc [n]

notifyNodeTypecheck :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeTypecheckerUpdate -> m ()
notifyNodeTypecheck loc n =
    sendUpdate $ TypecheckerUpdate $ NodeTCUpdate.Update loc n

notifyResultUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeId -> NodeResult.NodeValue -> Integer -> m ()
notifyResultUpdate loc nid v t =
    sendUpdate $ ResultUpdate $ NodeResult.Update loc nid v t

notifyCodeUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => FilePath -> Int -> Int -> Text -> Maybe Int -> m ()
notifyCodeUpdate path start end code cursor =
    sendUpdate $ CodeUpdate $ Substitute.Update path start end code cursor

sendUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => AsyncUpdate -> m ()
sendUpdate upd = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan upd

requestTC :: GraphLocation -> Graph -> Bool -> Command s ()
requestTC loc g flush = do
    chan <- asks $ view typecheckChan
    liftIO $ atomically $ writeTChan chan (loc, g, flush)
