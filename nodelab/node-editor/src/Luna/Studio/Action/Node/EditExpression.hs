module Luna.Studio.Action.Node.EditExpression
    ( editExpression
    ) where

import           Control.Arrow                               ((&&&))
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.Searcher                 as Searcher
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode)
import           Luna.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, expression, position)
import           Luna.Studio.State.Global                    (State)


editExpression :: NodeLoc -> Command State ()
editExpression nl = do
    mayNode <- getExpressionNode nl
    case (view position &&& view expression) <$> mayNode of
        Just (pos, expr) -> Searcher.openEdit expr nl pos
        _                -> return ()
