{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Empire.Data.Graph (
    Graph(..)
  , ast
  , nodeMapping
  , breadcrumbHierarchy
  , breadcrumbPortMapping
  , topLevelSeq
  , lastNameId
  , insideNode
  , NodeIDTarget(MatchNode, AnonymousNode)
  , getAnyRef
  , defaultGraph

  , withVis

  , AST
  , ASTState(..)
  ) where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy   as BC (empty)
import           Empire.Prelude

import           Empire.Data.AST                   (NodeRef)
import           Control.Monad.State               (MonadState(..), StateT, evalStateT, lift)
import           Empire.Data.Layers                (attachEmpireLayers)

import           Control.Monad.Raise                    (MonadException(..))
import           Luna.IR                                (IR, IRBuilder, AnyExpr, evalIRBuilder', evalPassManager',
                                                         attachLayer, snapshot, runRefCache, runRegs)
import qualified Luna.Pass.Manager                      as Pass (RefState)
import qualified Luna.Pass.Manager                      as PassManager (PassManager, RefCache, get)
import qualified Luna.Passes.Transform.Parsing.Parser   as Parser
import qualified Luna.Passes.Transform.Parsing.Parsing  ()
import qualified Luna.Passes.Transform.Parsing.CodeSpan as CodeSpan
import qualified Data.SpanTree                          as SpanTree
import           Data.TypeDesc                          (getTypeDesc)

import           System.Log                             (Logger, DropLogger, dropLogs)

import qualified Luna.IR.Repr.Vis           as Vis
import           Control.Monad              (void)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Maybe                 (isJust)
import           System.Environment         (lookupEnv)
import           Web.Browser                (openBrowser)


data Graph = Graph { _ast                   :: AST
                   , _nodeMapping           :: Map NodeId NodeIDTarget
                   , _breadcrumbHierarchy   :: BreadcrumbHierarchy
                   , _breadcrumbPortMapping :: Map NodeId (NodeId, NodeId)
                   , _topLevelSeq           :: Maybe NodeRef
                   , _lastNameId            :: Integer
                   , _insideNode            :: Maybe NodeId
                   } deriving Show

data NodeIDTarget = MatchNode     NodeRef
                  | AnonymousNode NodeRef
    deriving Show

getAnyRef :: NodeIDTarget -> NodeRef
getAnyRef (MatchNode ref)     = ref
getAnyRef (AnonymousNode ref) = ref


defaultGraph :: IO Graph
defaultGraph = do
    ast' <- defaultAST
    return $ Graph ast' Map.empty BC.empty Map.empty Nothing 0 Nothing

type AST      = ASTState
data ASTState = ASTState IR (Pass.RefState (PassManager.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (PassManager.RefCache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))))

instance Show ASTState where
    show _ = "AST"

instance MonadState Graph (PassManager.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (PassManager.RefCache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) where
    get   = (lift . lift . lift . lift . lift) get
    put   = (lift . lift . lift . lift . lift) . put
    state = (lift . lift . lift . lift . lift) . state

instance Exception e => MonadException e IO where
    raise = throwM

deriving instance MonadThrow m => MonadThrow (SpanTree.TreeBuilder s v m)

withVis :: MonadIO m => Vis.VisStateT m a -> m a
withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    let cfg = ByteString.unpack $ encode vis
    showVis <- liftIO $ lookupEnv "DEBUGVIS"
    if isJust showVis then void $ liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg else return ()
    return p

defaultAST :: IO AST
defaultAST = mdo
    let g = Graph ast Map.empty BC.empty Map.empty Nothing 0 Nothing
    ast <- flip evalStateT g $ withVis $ dropLogs $ runRefCache $ (\a -> SpanTree.runTreeBuilder a >>= \(foo, _) -> return foo) $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        CodeSpan.init
        attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
        Parser.init
        attachLayer 5 (getTypeDesc @Parser.Parser) (getTypeDesc @AnyExpr)
        attachEmpireLayers
        st <- snapshot
        pass <- PassManager.get
        return $ ASTState st pass
    return ast


makeLenses ''Graph
