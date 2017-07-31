module NodeEditor.Handler.MockMonads where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.MonadPath                  (MonadPath (MonadPath))
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.TypeRep                    (TypeRep (TCons))
import           NodeEditor.Action.State.NodeEditor         (getSelectedNodes, modifyNodeEditor)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.Model.NodeEditor          as NE
import           NodeEditor.State.Global                    (State)



handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.MockAddMonad _)) = Just $ do
    nids <- map (view (Node.nodeLoc . NodeLoc.nodeId)) <$> getSelectedNodes
    modifyNodeEditor $ NE.monads %= (MonadPath (TCons "IO" []) nids :)
handle (Shortcut (Shortcut.Event Shortcut.MockClearMonads _)) = Just $ modifyNodeEditor $ NE.monads .= def
handle _ = Nothing
