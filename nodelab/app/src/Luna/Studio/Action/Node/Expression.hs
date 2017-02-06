module Luna.Studio.Action.Node.Expression
    ( editExpression
    ) where

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Luna.Studio.Action.Command   (Command)
import qualified Luna.Studio.Action.Searcher  as Searcher
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as Model
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph


editExpression :: NodeId -> Command State ()
editExpression nodeId = do
    exprMay <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . Node.nodeType . Node.expression
    nodeMay <- Global.getNode nodeId
    withJust exprMay $ \expr -> withJust nodeMay $ \node -> do
        let pos = node ^. Model.position
        Searcher.openEdit expr nodeId pos
