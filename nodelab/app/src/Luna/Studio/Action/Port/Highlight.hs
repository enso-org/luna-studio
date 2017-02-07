module Luna.Studio.Action.Port.Highlight
    ( setHighlight
    ) where

import qualified Data.Set                     as Set
import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as NodeModel
import qualified Luna.Studio.React.Model.Port as PortModel
import           Luna.Studio.State.Action     (actionsBlockingPortHighlight)
import           Luna.Studio.State.Global     (State, runningActions)
import qualified Luna.Studio.State.Global     as Global

setHighlight :: AnyPortRef -> Bool -> Command State ()
setHighlight portRef highlight = do
    let nodeId  = portRef ^. PortRef.nodeId
    actions <- Set.fromList <$> runningActions
    when (highlight == False || Set.null (Set.intersection actions actionsBlockingPortHighlight)) $ do
        Global.modifyNode nodeId $ NodeModel.ports . ix portRef . PortModel.highlight .= highlight
