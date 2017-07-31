module NodeEditor.Action.Basic.CollapseToFunction where

import           Common.Action.Command              (Command)
import           Common.Prelude
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.NodeEditor (getSelectedNodes)
import           NodeEditor.React.Model.Node        (nodeLoc)
import           NodeEditor.State.Global            (State)



collapseToFunction :: Command State ()
collapseToFunction = Batch.collapseToFunction =<< view nodeLoc `fmap2` getSelectedNodes
