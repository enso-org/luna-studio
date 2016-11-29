module Empire.Data.Graph where

import           Data.Map.Lazy                     (Map)
import           Data.Tree                         (Forest)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy, empty)
import           Empire.Data.AST                   (AST, NodeRef)
import           Prologue

import           Old.Luna.Syntax.Model.Network.Builder (runNetworkBuilderT, star)

data Graph = Graph { _ast                 :: AST
                   , _nodeMapping         :: Map NodeId NodeRef
                   , _breadcrumbHierarchy :: BreadcrumbHierarchy
                   , _lastNameId          :: Integer
                   } deriving (Show)

makeLenses ''Graph

instance Default Graph where
    def = Graph defaultAST def empty 0

-- nextNodeId :: Graph -> Int
-- nextNodeId graph = IdGen.nextId $ graph ^. nodeMapping

defaultAST :: AST
defaultAST = snd (runIdentity $ runNetworkBuilderT def star :: (NodeRef, AST))
