module Empire.API.Data.AsyncUpdate where

import           Empire.API.Data.GraphLocation          (GraphLocation)
import           Empire.API.Data.Node                   (Node)
import           Prologue

import qualified Empire.API.Graph.ConnectUpdate         as ConnectUpdate
import qualified Empire.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

data AsyncUpdate = ConnectionUpdate ConnectUpdate.Update
                 | MonadsUpdate      MonadsUpdate.Update
                 | NodesUpdate        NodesUpdate.Update
                 | TypecheckerUpdate NodeTCUpdate.Update
                 | ResultUpdate        NodeResult.Update
                 deriving (Show, Eq)
