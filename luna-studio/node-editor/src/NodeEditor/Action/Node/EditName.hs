module NodeEditor.Action.Node.EditName where

import           Common.Prelude
import           Control.Arrow                              ((&&&))
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.Searcher                 as Searcher
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, name, position)
import           NodeEditor.State.Global                    (State)


editName :: NodeLoc -> Command State ()
editName nl = withJustM (fmap2 (view name) $ getExpressionNode nl) $ Searcher.openEditName nl
