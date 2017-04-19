module Node.Editor.Action.Basic.SetNodeCode where

import qualified Node.Editor.Action.Batch                    as Batch
import           Node.Editor.Action.Command                  (Command)
import qualified Node.Editor.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc, code)
import           Node.Editor.State.Global                    (State)


setNodeCode :: NodeLoc -> Text -> Command State ()
setNodeCode nl update =
    whenM (localSetNodeCode nl update) $ Batch.setNodeCode nl update

localSetNodeCode :: NodeLoc -> Text -> Command State Bool
localSetNodeCode nl update = do
    NodeEditor.modifyExpressionNode nl $ code .= Just update
    NodeEditor.inGraph nl
