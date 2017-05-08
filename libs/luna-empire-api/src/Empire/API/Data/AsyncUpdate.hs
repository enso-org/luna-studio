module Empire.API.Data.AsyncUpdate where

import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Prologue

import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

data AsyncUpdate = MonadsUpdate      MonadsUpdate.Update
                 | TypecheckerUpdate NodeTCUpdate.Update
                 | ResultUpdate        NodeResult.Update
                 | CodeUpdate          Substitute.Update
                 deriving (Show, Eq)
