{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Empire where

import           Empire.Data.AST               (SomeASTException)
import           Empire.Data.Graph             (ClsGraph, Graph)
import           Empire.Data.Library           (Library)
import           Empire.Prelude                hiding (TypeRep)
import           Empire.Prelude
import           Luna.Builtin.Data.Function    (Function)
import           Luna.Builtin.Data.Module      (Imports)
import           Luna.Compilation              (CompiledModules)
import           LunaStudio.API.AsyncUpdate    (AsyncUpdate)
import qualified LunaStudio.Data.Error         as APIError
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (ExpressionNode, NodeId)
import           LunaStudio.Data.PortDefault   (PortValue)
import           LunaStudio.Data.Project       (ProjectId)
import           LunaStudio.Data.TypeRep       (TypeRep)
import           OCI.IR.Name                   (Name)

import           Control.Concurrent.Async      (Async)
import           Control.Concurrent.MVar       (MVar)
import           Control.Concurrent.STM.TChan  (TChan)
import           Control.Exception             (try)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Lazy                 (Map)
import qualified Data.Map.Lazy                 as Map

type Error = String

type ActiveFiles = Map FilePath Library

data SymbolMap = SymbolMap { _functions :: [Text]
                           , _classes   :: Map Text [Text]
                           } deriving (Show, Eq)
makeLenses ''SymbolMap

instance Default SymbolMap where
    def = SymbolMap def def

data Env = Env { _activeFiles       :: ActiveFiles
               , _activeInterpreter :: Bool
               } deriving (Show)
makeLenses ''Env

instance Default Env where
    def = Env Map.empty True

data TCRequest = TCRequest { _tcLocation       :: GraphLocation
                           , _tcGraph          :: ClsGraph
                           , _tcFlush          :: Bool
                           , _tcRunInterpreter :: Bool
                           , _tcRecompute      :: Bool
                           , _tcStop           :: Bool
                           }
makeLenses ''TCRequest

data CommunicationEnv = CommunicationEnv { _updatesChan   :: TChan AsyncUpdate
                                         , _typecheckChan :: MVar TCRequest
                                         , _scopeVar      :: MVar SymbolMap
                                         , _modules       :: MVar CompiledModules
                                         } deriving Generic
makeLenses ''CommunicationEnv

instance Show CommunicationEnv where
    show _ = "CommunicationEnv"

data InterpreterEnv = InterpreterEnv { _valuesCache :: Map NodeId [PortValue]
                                     , _nodesCache  :: Map NodeId ExpressionNode
                                     , _errorsCache :: Map NodeId (APIError.Error APIError.NodeError)
                                     , _graph       :: ClsGraph
                                     , _cleanUp     :: IO ()
                                     , _listeners   :: [Async ()]
                                     }
makeLenses ''InterpreterEnv

type CommandStack s = ReaderT CommunicationEnv (StateT s IO)
type Command s a = ReaderT CommunicationEnv (StateT s IO) a

type Empire a = Command Env a

runEmpire :: CommunicationEnv -> s -> Command s a -> IO (a, s)
runEmpire notif st cmd = runStateT (runReaderT cmd notif) st

execEmpire :: CommunicationEnv -> s -> Command s a -> IO a
execEmpire = fmap fst .:. runEmpire

evalEmpire :: CommunicationEnv -> s -> Command s a -> IO s
evalEmpire = fmap snd .:. runEmpire

empire :: (CommunicationEnv -> s -> IO (a, s)) -> Command s a
empire = ReaderT . fmap StateT
