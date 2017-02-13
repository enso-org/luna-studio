module Empire.API.Data.AsyncUpdate where

import Prologue
import Empire.API.Data.GraphLocation (GraphLocation)
import Empire.API.Data.Node          (Node)

import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.NodesUpdate           as NodesUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResult
import qualified Empire.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate

data AsyncUpdate = ConnectionUpdate       Connect.Update
                 | NodesUpdate        NodesUpdate.Update
                 | TypecheckerUpdate NodeTCUpdate.Update
                 | ResultUpdate        NodeResult.Update
                 deriving (Show, Eq)
