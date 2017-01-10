module Luna.Studio.Action.Node.Expand
    ( expandSelectedNodes
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import           Luna.Studio.React.Store            (widget)
import           Luna.Studio.React.Store            (WRef (_ref))
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)

expandSelectedNodes :: Command State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $
        Store.modify_ update . _ref
