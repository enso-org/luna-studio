{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.Data.Graph (
    Graph(..)
  , ast
  , breadcrumbHierarchy
  , lastNameId
  , defaultGraph
  , withVis
  , AST
  , ASTState(..)
  ) where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BItem)
import           Empire.Prelude

import           Empire.Data.AST                   (NodeRef)
import           Control.Monad.State               (MonadState(..), StateT, evalStateT, lift)
import           Empire.Data.Layers                (attachEmpireLayers)
import qualified Control.Monad.State.Dependent     as DepState

import           Control.Monad.Raise                    (MonadException(..))
import           Luna.IR                                (IR, IRBuilder, AnyExpr, evalIRBuilder', evalPassManager',
                                                         attachLayer, snapshot, runRegs, Cache)
import qualified OCI.Pass.Manager                       as Pass (RefState)
import qualified OCI.Pass.Manager                       as PassManager (PassManager, State)
import qualified Luna.Syntax.Text.Parser.Parser         as Parser
import qualified Luna.Syntax.Text.Parser.Parsing        as Parser ()
import qualified Luna.Syntax.Text.Parser.CodeSpan       as CodeSpan
import qualified Data.SpanTree                          as SpanTree
import           Data.TypeDesc                          (getTypeDesc)

import           System.Log                             (Logger, DropLogger, dropLogs, MonadLogging)

import qualified OCI.IR.Repr.Vis            as Vis
import           Control.Monad              (void)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Maybe                 (isJust)
import           System.Environment         (lookupEnv)
import           Web.Browser                (openBrowser)


data Graph = Graph { _ast                   :: AST
                   , _breadcrumbHierarchy   :: BItem
                   , _lastNameId            :: Integer
                   } deriving Show

defaultGraph :: IO Graph
defaultGraph = do
    ast' <- defaultAST
    return $ Graph ast' def 0

type AST      = ASTState
data ASTState = ASTState IR (Pass.RefState (PassManager.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))))

instance Show ASTState where
    show _ = "AST"

instance MonadState s m => MonadState s (DepState.StateT b m) where
    get = lift   get
    put = lift . put

instance MonadState s m => MonadState s (PassManager.PassManager m) where
    get = lift   get
    put = lift . put

instance MonadState s m => MonadState s (SpanTree.TreeBuilder x y m) where
    get = lift   get
    put = lift . put

instance MonadState s m => MonadState s (Logger DropLogger m) where
    get = lift   get
    put = lift . put

instance Exception e => MonadException e IO where
    raise = throwM

deriving instance MonadThrow m => MonadThrow (SpanTree.TreeBuilder s v m)

withVis :: MonadIO m => Vis.VisStateT m a -> m a
withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    when (not . null $ vis ^. Vis.steps) $ do
        let cfg = ByteString.unpack $ encode vis
        showVis <- liftIO $ lookupEnv "DEBUGVIS"
        if isJust showVis then void $ liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg else return ()
    return p

defaultAST :: IO AST
defaultAST = mdo
    let g = Graph ast def 0
    ast <- flip evalStateT g $ withVis $ dropLogs $ DepState.evalDefStateT @Cache $ (\a -> SpanTree.runTreeBuilder a >>= \(foo, _) -> return foo) $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        CodeSpan.init
        attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
        Parser.init
        attachLayer 5 (getTypeDesc @Parser.Parser) (getTypeDesc @AnyExpr)
        attachEmpireLayers
        st   <- snapshot
        pass <- DepState.get @PassManager.State
        return $ ASTState st pass
    return ast


makeLenses ''Graph
