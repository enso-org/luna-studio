module Empire.Commands.Publisher where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan               (writeTChan)
import           Control.Monad.Reader                       hiding (liftIO)
import           Control.Monad.STM                          (atomically)
import           Data.Text                                  (Text)
import           Empire.Data.Graph                          (Graph, ClsGraph)
import           Empire.Empire
import           Empire.Prelude
import           LunaStudio.API.AsyncUpdate                 (AsyncUpdate (..))
import           LunaStudio.Data.Diff                       (Diff(..))
import           LunaStudio.Data.GraphLocation              (GraphLocation)
import           LunaStudio.Data.MonadPath                  (MonadPath)
import           LunaStudio.Data.Node                       (NodeId, NodeTypecheckerUpdate)
import           LunaStudio.Data.NodeValue                  (NodeValue)

import qualified LunaStudio.API.Atom.Substitute             as Substitute
import qualified LunaStudio.API.Control.Interpreter         as Interpreter
import qualified LunaStudio.API.Graph.MonadsUpdate          as Monads
import qualified LunaStudio.API.Graph.NodeResultUpdate      as NodeResult
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import           LunaStudio.Data.Point                      (Point)


notifyMonadsUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> [MonadPath] -> m ()
notifyMonadsUpdate loc m =
    sendUpdate $ MonadsUpdate $ Monads.Update loc m

notifyNodeTypecheck :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeTypecheckerUpdate -> m ()
notifyNodeTypecheck loc n =
    sendUpdate $ TypecheckerUpdate $ NodeTCUpdate.Update loc n

notifyResultUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeId -> NodeValue -> Integer -> m ()
notifyResultUpdate loc nid v t =
    sendUpdate $ ResultUpdate $ NodeResult.Update loc nid v t

notifyCodeUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => FilePath -> Text -> Maybe Point -> m ()
notifyCodeUpdate path code cursor =
    sendUpdate $ CodeUpdate $ Substitute.Update path [Diff Nothing code cursor]

notifyInterpreterUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => Text -> m ()
notifyInterpreterUpdate msg =
    sendUpdate $ InterpreterUpdate $ Interpreter.Update msg

sendUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => AsyncUpdate -> m ()
sendUpdate upd = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan upd

requestTC :: GraphLocation -> ClsGraph -> Bool -> Bool -> Command s ()
requestTC loc g flush runInterpreter = do
    chan <- view typecheckChan
    liftIO $ do
        tryTakeMVar chan
        putMVar chan $ TCRequest loc g flush runInterpreter
