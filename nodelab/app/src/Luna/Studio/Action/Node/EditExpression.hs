module Luna.Studio.Action.Node.EditExpression
    ( editExpression
    ) where

import           Control.Arrow                               ((&&&))
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.Searcher                 as Searcher
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeId, expression, position)
import           Luna.Studio.State.Global                    (State)


editExpression :: NodeId -> Command State ()
editExpression nid = do
    mayNode <- getExpressionNode nid
    case (view position &&& view expression) <$> mayNode of
        Just (pos, expr) -> Searcher.openEdit expr nid pos
        _                -> return ()
