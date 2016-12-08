module Empire.Empire where

import           Empire.API.Data.AsyncUpdate   (AsyncUpdate)
import           Empire.API.Data.DefaultValue  (Value)
import qualified Empire.API.Data.Error         as APIError
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.Project       (ProjectId)
import           Empire.API.Data.TypeRep       (TypeRep)
import           Empire.Data.Graph             (Graph)
import           Empire.Data.Project           (Project)
import           Prologue                      hiding (TypeRep)

import           Control.Concurrent.STM.TChan  (TChan)
import           Control.Monad.Error           (ErrorT (..), MonadError, runErrorT, throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Lazy                 (Map)
import qualified Data.Map.Lazy                 as Map

type Error = String

type ProjectManager = Map ProjectId Project

newtype Env = Env { _projectManager :: ProjectManager } deriving Show
makeLenses ''Env

instance Default Env where
    def = Env Map.empty

data CommunicationEnv = CommunicationEnv { _updatesChan   :: TChan AsyncUpdate
                                         -- FIXME[MK]: Yeah, let's use 3-tuples, way to code!
                                         , _typecheckChan :: TChan (GraphLocation, Graph, Bool)
                                         }
makeLenses ''CommunicationEnv

instance Show CommunicationEnv where
    show _ = "CommunicationEnv"

data InterpreterEnv = InterpreterEnv { _valuesCache :: Map NodeId [Value]
                                     , _nodesCache  :: Map NodeId Node
                                     , _errorsCache :: Map NodeId (APIError.Error TypeRep)
                                     , _graph       :: Graph
                                     , _destructors :: [IO ()]
                                     }
makeLenses ''InterpreterEnv

instance Default InterpreterEnv where
    def = InterpreterEnv def def def def def

type Command s a = ErrorT Error (ReaderT CommunicationEnv (StateT s IO)) a

type Empire a = Command Env a

runEmpire :: CommunicationEnv -> s -> Command s a -> IO (Either Error a, s)
runEmpire notif st cmd = runStateT (runReaderT (runErrorT cmd) notif) st

execEmpire :: CommunicationEnv -> s -> Command s a -> IO (Either Error a)
execEmpire = fmap fst .:. runEmpire

empire :: (CommunicationEnv -> s -> IO (Either Error a, s)) -> Command s a
empire = ErrorT . ReaderT . fmap StateT

infixr 4 <?!>
(<?!>) :: MonadError Error m => m (Maybe a) -> Error -> m a
(<?!>) cmd err = cmd >>= maybe (throwError err) return
