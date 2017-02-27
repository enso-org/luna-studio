module Luna.Studio.Action.Node.Code
    ( setCode
    ) where

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as Model
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph


setCode :: NodeId -> Text -> Command State ()
setCode nodeId code = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.code .= Just code
    Global.modifyNode nodeId $ Model.code .= Just code
