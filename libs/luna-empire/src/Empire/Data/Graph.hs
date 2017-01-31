{-# LANGUAGE RecursiveDo #-}

module Empire.Data.Graph (
    Graph(..)
  , ast
  , nodeMapping
  , breadcrumbHierarchy
  , breadcrumbPortMapping
  , lastNameId
  , insideNode
  , NodeIDTarget(MatchNode, AnonymousNode)
  , getAnyRef
  , defaultGraph

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
import           Control.Monad.State (MonadState(..), StateT, evalStateT, lift)
import           Empire.Data.Layers  (attachEmpireLayers)

import           Control.Monad.Raise (MonadException(..))
import           Luna.IR             (IR, IRBuilder, evalIRBuilder', evalPassManager',
                                      snapshot, runRefCache, runRegs)
import qualified Luna.Pass.Manager   as Pass (RefState)
import qualified Luna.Pass.Manager   as PassManager (PassManager, RefCache, get)

import           System.Log          (Logger, DropLogger, dropLogs)

data Graph = Graph { _ast                   :: AST
                   , _nodeMapping           :: Map NodeId NodeIDTarget
                   , _breadcrumbHierarchy   :: BreadcrumbHierarchy
                   , _breadcrumbPortMapping :: Map NodeId (NodeId, NodeId)
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
    return $ Graph ast' Map.empty BC.empty Map.empty 0 Nothing

type AST           = ASTState
data ASTState = ASTState IR (Pass.RefState (PassManager.PassManager (IRBuilder (PassManager.RefCache (Logger DropLogger (StateT Graph IO))))))

instance Show ASTState where
    show _ = "AST"

instance MonadState Graph (PassManager.PassManager (IRBuilder (PassManager.RefCache (Logger DropLogger (StateT Graph IO))))) where
    get = (lift . lift . lift . lift) get
    put = (lift . lift . lift . lift) . put
    state = (lift . lift . lift . lift) . state

instance Exception e => MonadException e IO where
    raise = throwM

defaultAST :: IO AST
defaultAST = mdo
    let g = Graph ast Map.empty BC.empty Map.empty 0 Nothing
    ast <- flip evalStateT g $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        attachEmpireLayers
        st <- snapshot
        pass <- PassManager.get
        return $ ASTState st pass
    return ast


makeLenses ''Graph
