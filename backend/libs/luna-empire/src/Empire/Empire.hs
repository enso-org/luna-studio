module Empire.Empire where

import Empire.Prelude

import qualified Data.Graph.Store                      as Store
import qualified Data.Map.Lazy                         as Map
import qualified Empire.Data.Graph                     as Graph
import qualified Luna.IR                               as IR
import qualified Luna.Package.Configuration.Local      as Local
import qualified Luna.Pass.Resolve.Data.Resolution     as Res
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Typing.Data.Typed           as Typed
import qualified Luna.Runtime.Data.Evaluated           as Runtime
import qualified LunaStudio.Data.Searcher.Hint.Library as Libraries

import Control.Concurrent.Async      (Async)
import Control.Concurrent.MVar       (MVar)
import Control.Concurrent.STM.TChan  (TChan)
import Control.Monad.Reader          (ReaderT (ReaderT), ask, runReaderT)
import Control.Monad.State           (StateT (StateT), get, put, runStateT)
import Data.Map.Lazy                 (Map)
import Empire.Data.AST               (NodeRef)
import Empire.Data.Graph             (ClsGraph, CommandState (CommandState))
import Empire.Data.Library           (Library)
import LunaStudio.API.AsyncUpdate    (AsyncUpdate)
import LunaStudio.Data.GraphLocation (GraphLocation)
import Path                          (Path, Abs, Rel, Dir, File)


type Error = String

type ActiveFiles = Map (Path Rel File) Library

data SymbolMap = SymbolMap
    { _functions :: [Text]
    , _classes   :: Map Text [Text]
    } deriving (Show, Eq)
makeLenses ''SymbolMap

instance Default SymbolMap where
    def = SymbolMap def def

data Env = Env
    { _activeProject     :: Path Abs Dir
    , _activeFiles       :: ActiveFiles
    , _activeInterpreter :: Bool
    } deriving (Show)
makeLenses ''Env

mkEnv :: Path Abs Dir -> Env
mkEnv p = Env p def True

data TCRunRequest = TCRunRequest
    { _tcLocation       :: GraphLocation
    , _tcGraph          :: ClsGraph
    , _rooted           :: Store.RootedWithRedirects NodeRef
    , _tcRunInterpreter :: Bool
    , _tcRecompute      :: Bool
    }
makeLenses ''TCRunRequest

data TCRequest
    = TCRun TCRunRequest
    | TCStop
    | TCSetProject (Path Abs Dir)
makePrisms ''TCRequest

data CommunicationEnv = CommunicationEnv
    { _updatesChan   :: TChan AsyncUpdate
    , _typecheckChan :: MVar TCRequest
    } deriving Generic
makeLenses ''CommunicationEnv

instance Show CommunicationEnv where
    show _ = "CommunicationEnv"

data InterpreterEnv = InterpreterEnv
    { _cleanUp       :: IO ()
    , _listeners     :: [Async ()]
    , _mappedUnits   :: Map IR.Qualified Unit.Unit
    , _typedUnits    :: Typed.Units
    , _runtimeUnits  :: Runtime.Units
    , _resolvers     :: Map IR.Qualified Res.UnitResolver
    , _packageRoot   :: Path Abs Dir
    , _packageConfig :: Local.Config
    }
makeLenses ''InterpreterEnv

type CommandStack s = ReaderT CommunicationEnv (StateT (CommandState s) IO)

type Command s a = ReaderT CommunicationEnv (StateT (CommandState s) IO) a

type Empire a = Command Env a

runEmpire :: CommunicationEnv -> CommandState s -> Command s a
    -> IO (a, CommandState s)
runEmpire notif st cmd = runStateT (runReaderT cmd notif) st

evalEmpire :: CommunicationEnv -> CommandState s -> Command s a -> IO a
evalEmpire = fmap fst .:. runEmpire

execEmpire :: CommunicationEnv -> CommandState s -> Command s a
    -> IO (CommandState s)
execEmpire = fmap snd .:. runEmpire

empire :: (CommunicationEnv -> CommandState s -> IO (a, CommandState s))
    -> Command s a
empire = ReaderT . fmap StateT

zoomCommand :: Lens' s t -> Command t a -> Command s a
zoomCommand l cmd = do
    CommandState pmState s <- get
    commEnv                <- ask
    (r, CommandState pm' newS) <- liftIO $
        runEmpire commEnv (CommandState pmState $ s ^. l) cmd
    put $ CommandState pm' $ s & l .~ newS
    pure r

zoomCommandMaybe :: Command s a -> Command (Maybe s) (Maybe a)
zoomCommandMaybe cmd = do
    CommandState pmState s <- get
    communicationsEnv      <- ask
    for s $ \state -> do
        (r, newS) <- liftIO $ runEmpire communicationsEnv
            (CommandState pmState state) cmd
        put $ newS & Graph.userState %~ Just
        pure r

generalizeCommand :: Lens' t s -> (s -> t) -> Command t a -> Command s a
generalizeCommand lens constructor cmd = do
    CommandState pmState s <- get
    communicationsEnv      <- ask
    (r, CommandState pm' newS) <- liftIO $
        runEmpire communicationsEnv (CommandState pmState $ constructor s) cmd
    put $ CommandState pm' (newS ^. lens)
    pure r

