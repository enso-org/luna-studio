module NodeEditor.Action.Node.EditExpression
    ( editExpression
    ) where

import           Common.Prelude
import           Control.Arrow                              ((&&&))
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.Searcher                 as Searcher
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, expression, position)
import           NodeEditor.State.Global                    (State)


editExpression :: NodeLoc -> Command State ()
editExpression nl = do
    mayNode <- getExpressionNode nl
    case (view position &&& view expression) <$> mayNode of
        Just (pos, expr) -> Searcher.openEditExpression expr nl pos
        _                -> return ()
