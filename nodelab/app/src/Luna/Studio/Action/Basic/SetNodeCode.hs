module Luna.Studio.Action.Basic.SetNodeCode where

import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, code)
import           Luna.Studio.State.Global                    (State)


setNodeCode :: NodeLoc -> Text -> Command State ()
setNodeCode nl update =
    whenM (localSetNodeCode nl update) $ Batch.setNodeCode nl update

localSetNodeCode :: NodeLoc -> Text -> Command State Bool
localSetNodeCode nl update = do
    NodeEditor.modifyExpressionNode nl $ code .= Just update
    NodeEditor.inGraph nl
