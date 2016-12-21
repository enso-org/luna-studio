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
  ) where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy   as BC (empty)
import           Empire.Prelude

import           Empire.Data.AST                   (AST, NodeRef, defaultAST)


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

makeLenses ''Graph

defaultGraph :: IO Graph
defaultGraph = do
    ast' <- defaultAST
    return $ Graph ast' Map.empty BC.empty Map.empty 0 Nothing
