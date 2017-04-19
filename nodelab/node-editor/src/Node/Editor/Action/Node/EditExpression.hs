module Node.Editor.Action.Node.EditExpression
    ( editExpression
    ) where

import           Control.Arrow                               ((&&&))
import           Node.Editor.Action.Command                  (Command)
import qualified Node.Editor.Action.Searcher                 as Searcher
import           Node.Editor.Action.State.NodeEditor         (getExpressionNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc, expression, position)
import           Node.Editor.State.Global                    (State)


editExpression :: NodeLoc -> Command State ()
editExpression nl = do
    mayNode <- getExpressionNode nl
    case (view position &&& view expression) <$> mayNode of
        Just (pos, expr) -> Searcher.openEdit expr nl pos
        _                -> return ()
