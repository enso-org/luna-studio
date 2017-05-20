module NodeEditor.Action.Node.EditExpression
    ( editExpression
    ) where

import           Common.Prelude
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.Searcher                 as Searcher
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, expression)
import           NodeEditor.State.Global                    (State)


editExpression :: NodeLoc -> Command State ()
editExpression nl = withJustM (fmap2 (view expression) $ getExpressionNode nl) $ Searcher.openEditExpression nl
