{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.Data.Graph where

import           Empire.Data.BreadcrumbHierarchy   (HasRefs(..), LamItem)
import           Empire.Prelude

import           Control.Monad.State               (MonadState(..), lift)
import qualified Data.Graph.Data.Graph.Class       as LunaGraph
import           Data.Map                          (Map)
import           Data.Text.Position                (Delta)
import           Empire.Data.AST                   (NodeRef)

import qualified Control.Monad.State.Layered       as DepState
import           Luna.Pass.Data.Stage              (Stage)
import           Luna.Syntax.Text.Parser.State.Marker (ID)

import           LunaStudio.Data.Node              (NodeId)
import           LunaStudio.Data.NodeCache
import qualified OCI.Pass.Management.Scheduler     as Scheduler

-- import qualified OCI.IR.Repr.Vis            as Vis
-- import           Data.Aeson                        (encode)
-- import qualified Data.ByteString.Lazy.Char8        as ByteString
-- import           Data.Maybe                        (isJust)
-- import           System.Environment                (lookupEnv)
-- import           Web.Browser                       (openBrowser)

type MarkerId = ID

data CommandState s = CommandState { _pmState   :: PMState
                                   , _userState :: s
                                   }

instance Show s => Show (CommandState s) where
    show (CommandState _ s) = "CommandState { " <> show s <> "}"

data PMState = PMState { _pmScheduler :: Scheduler.State
                       , _pmStage     :: LunaGraph.State Stage
                       }

makeLenses ''CommandState
makeLenses ''PMState

defaultPMState :: IO PMState
defaultPMState = do
    (scState, grState) <- LunaGraph.encodeAndEval @Stage $ do
        ((), schedulerState) <- Scheduler.runT $ return ()
        graphState <- LunaGraph.getState
        return (schedulerState, graphState)
    return $ PMState scState grState


instance Show PMState where show _ = "PMState"


data Graph = Graph { _breadcrumbHierarchy   :: LamItem
                   , _codeMarkers           :: Map MarkerId NodeRef
                   , _globalMarkers         :: Map MarkerId NodeRef
                   , _graphCode             :: Text
                   , _parseError            :: Maybe SomeException
                   , _fileOffset            :: Delta
                   , _graphNodeCache        :: NodeCache
                   } deriving Show

data FunctionGraph = FunctionGraph { _funName    :: String
                                   , _funGraph   :: Graph
                                   , _funMarkers :: Map MarkerId NodeRef
                                   } deriving Show

data ClsGraph = ClsGraph { _clsClass       :: NodeRef
                         , _clsCodeMarkers :: Map MarkerId NodeRef
                         , _clsCode        :: Text
                         , _clsParseError  :: Maybe SomeException
                         , _clsFuns        :: Map NodeId FunctionGraph
                         , _clsNodeCache   :: NodeCache
                         } deriving Show

instance HasRefs Graph where
    refs fun (Graph a b c d e f g) = Graph <$> refs fun a <*> traverse fun b <*> traverse fun c <*> pure d <*> pure e <*> pure f <*> pure g

instance HasRefs FunctionGraph where
    refs fun (FunctionGraph a b c) = FunctionGraph a <$> refs fun b <*> traverse fun c

instance HasRefs ClsGraph where
    refs fun (ClsGraph a b c d e f) = ClsGraph <$> fun a <*> traverse fun b <*> pure c <*> pure d <*> (traverse.refs) fun e <*> pure f

instance MonadState s m => MonadState s (DepState.StateT b m) where
    get = lift   get
    put = lift . put

makeLenses ''Graph
makeLenses ''FunctionGraph
makeLenses ''ClsGraph

-- unescapeUnaryMinus :: Vis.Node -> Vis.Node
-- unescapeUnaryMinus n = if n ^. Vis.name == "Var \"#uminus#\""
--                        then n & Vis.name .~ "Var uminus"
--                        else n

-- withVis :: MonadIO m => Vis.VisStateT m a -> m a
-- withVis m = do
--     (p, vis) <- Vis.newRunDiffT m
--     when (not . null $ vis ^. Vis.steps) $ do
--         let unescaped = vis & Vis.nodes %~ Map.map unescapeUnaryMinus
--             cfg = ByteString.unpack $ encode unescaped
--         showVis <- liftIO $ lookupEnv "DEBUGVIS"
--         if isJust showVis then void $ liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg else return ()
--     return p
withVis :: a -> a
withVis = id

snapshot :: IO ()
snapshot = return ()


class HasCode g where
    code :: Lens' g Text

instance HasCode (Graph) where
    code = graphCode

instance HasCode (ClsGraph) where
    code = clsCode

instance HasCode a => HasCode (CommandState a) where
    code = userState . code

class HasNodeCache g where
    nodeCache :: Lens' g NodeCache

instance HasNodeCache Graph where
    nodeCache = graphNodeCache

instance HasNodeCache ClsGraph where
    nodeCache = clsNodeCache

instance HasNodeCache a => HasNodeCache (CommandState a) where
    nodeCache = userState . nodeCache