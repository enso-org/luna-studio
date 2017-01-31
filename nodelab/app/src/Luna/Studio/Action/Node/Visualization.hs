module Luna.Studio.Action.Node.Visualization
    ( visualizationsToggled
    ) where

import           Empire.API.Data.Node             (NodeId)
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Node.NodeMeta (modifyNodeMeta)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node     as Model
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global


visualizationsToggled :: NodeId -> Bool -> Command State ()
visualizationsToggled nid val = do
    modifyNodeMeta nid $ NodeMeta.displayResult .~ val
    Global.modifyNode nid $ Model.visualizationsEnabled .= val
