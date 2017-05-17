module Empire.Commands.Publisher where

import           Control.Concurrent.STM.TChan               (writeTChan)
import           Control.Monad.Reader
import           Control.Monad.STM                          (atomically)
import           Data.Text                                  (Text)
import           Empire.Data.Graph                          (Graph)
import           Empire.Empire
import           Empire.Prelude
import           LunaStudio.API.AsyncUpdate                 (AsyncUpdate (..))
import           LunaStudio.Data.GraphLocation              (GraphLocation)
import           LunaStudio.Data.MonadPath                  (MonadPath)
import           LunaStudio.Data.Node                       (ExpressionNode, NodeId, NodeTypecheckerUpdate)
import           LunaStudio.Data.TypeRep                    (TypeRep)

import qualified LunaStudio.API.Atom.Substitute             as Substitute
import qualified LunaStudio.API.Atom.GetBuffer              as GetBuffer
import qualified LunaStudio.API.Graph.MonadsUpdate          as Monads
import qualified LunaStudio.API.Graph.NodeResultUpdate      as NodeResult
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

notifyMonadsUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> [MonadPath] -> m ()
notifyMonadsUpdate loc m =
    sendUpdate $ MonadsUpdate $ Monads.Update loc m

notifyNodeTypecheck :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeTypecheckerUpdate -> m ()
notifyNodeTypecheck loc n =
    sendUpdate $ TypecheckerUpdate $ NodeTCUpdate.Update loc n

notifyResultUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => GraphLocation -> NodeId -> NodeResult.NodeValue -> Integer -> m ()
notifyResultUpdate loc nid v t =
    sendUpdate $ ResultUpdate $ NodeResult.Update loc nid v t

notifyCodeUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => FilePath -> Int -> Int -> Text -> Maybe Int -> [(Int, [String])] -> m ()
notifyCodeUpdate path start end code cursor tags =
    sendUpdate $ CodeUpdate $ Substitute.Update path start end code cursor tags

notifyLexerUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => FilePath -> [(Int, [String])] -> m ()
notifyLexerUpdate path lexer = sendUpdate $ LexerUpdate $ GetBuffer.LexUpdate path lexer

sendUpdate :: (MonadReader CommunicationEnv m, MonadIO m) => AsyncUpdate -> m ()
sendUpdate upd = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan upd

requestTC :: GraphLocation -> Graph -> Bool -> Command s ()
requestTC loc g flush = do
    chan <- asks $ view typecheckChan
    liftIO $ atomically $ writeTChan chan (loc, g, flush)
