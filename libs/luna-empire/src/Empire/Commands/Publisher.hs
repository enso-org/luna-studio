module Empire.Commands.Publisher where

import           Control.Concurrent.STM.TChan           (writeTChan)
import           Control.Monad.Reader
import           Control.Monad.STM                      (atomically)
import           Empire.API.Data.AsyncUpdate            (AsyncUpdate (..))
import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.Node                   (Node, NodeId, NodeTypecheckerUpdate)
import           Empire.API.Data.TypeRep                (TypeRep)
import           Empire.Data.Graph                      (Graph)
import           Empire.Empire
import           Empire.Prelude

import qualified Empire.API.Graph.MonadsUpdate          as Monads
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified Empire.API.Graph.NodesUpdate           as Node
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

notifyMonadsUpdate :: GraphLocation -> [MonadPath] -> Command s ()
notifyMonadsUpdate loc m =
    sendUpdate $ MonadsUpdate $ Monads.Update loc m

notifyNodeUpdate :: GraphLocation -> Node -> Command s ()
notifyNodeUpdate loc n =
    sendUpdate $ NodesUpdate $ Node.Update loc [n]

notifyNodeTypecheck :: GraphLocation -> NodeTypecheckerUpdate -> Command s ()
notifyNodeTypecheck loc n =
    sendUpdate $ TypecheckerUpdate $ NodeTCUpdate.Update loc n

notifyResultUpdate :: GraphLocation -> NodeId -> NodeResult.NodeValue -> Integer -> Command s ()
notifyResultUpdate loc nid v t =
    sendUpdate $ ResultUpdate $ NodeResult.Update loc nid v t

sendUpdate :: AsyncUpdate -> Command s ()
sendUpdate upd = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan upd

requestTC :: GraphLocation -> Graph -> Bool -> Command s ()
requestTC loc g flush = do
    chan <- asks $ view typecheckChan
    liftIO $ atomically $ writeTChan chan (loc, g, flush)
