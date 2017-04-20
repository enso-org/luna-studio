module NodeEditor.Action.Basic.SetNodeCode where

import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           Common.Prelude
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, code)
import           NodeEditor.State.Global                    (State)


setNodeCode :: NodeLoc -> Text -> Command State ()
setNodeCode nl update =
    whenM (localSetNodeCode nl update) $ Batch.setNodeCode nl update

localSetNodeCode :: NodeLoc -> Text -> Command State Bool
localSetNodeCode nl update = do
    NodeEditor.modifyExpressionNode nl $ code .= Just update
    NodeEditor.inGraph nl
