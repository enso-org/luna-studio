module Empire.Data.Graph where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy, empty)
import           Empire.Data.AST                   (AST, NodeRef)
import           Prologue

import           Old.Luna.Syntax.Model.Network.Builder (runNetworkBuilderT, star)

data Graph = Graph { _ast                   :: AST
                   , _nodeMapping           :: Map NodeId NodeIDTarget
                   , _breadcrumbHierarchy   :: BreadcrumbHierarchy
                   , _breadcrumbPortMapping :: Map NodeId (NodeId, NodeId)
                   , _lastNameId            :: Integer
                   , _insideNode            :: Maybe NodeId
                   } deriving (Show)

data NodeIDTarget = MatchNode     NodeRef
                  | AnonymousNode NodeRef
    deriving Show

getAnyRef :: NodeIDTarget -> NodeRef
getAnyRef (MatchNode ref)     = ref
getAnyRef (AnonymousNode ref) = ref

makeLenses ''Graph

instance Default Graph where
    def = Graph defaultAST def empty Map.empty 0 Nothing

defaultAST :: AST
defaultAST = snd (runIdentity $ runNetworkBuilderT def star :: (NodeRef, AST))
