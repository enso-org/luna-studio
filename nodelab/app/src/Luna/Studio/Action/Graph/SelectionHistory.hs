module Luna.Studio.Action.Graph.SelectionHistory
    ( dropSelectionHistory
    , modifySelectionHistory
    ) where

import qualified Data.Set                   as Set
import           Empire.API.Data.Node       (NodeId)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)
import qualified Luna.Studio.State.Global   as Global


historyMaxLength :: Int
historyMaxLength = 10

dropSelectionHistory :: Command State ()
dropSelectionHistory = Global.selectionHistory .= def

modifySelectionHistory :: [NodeId] -> Command State ()
modifySelectionHistory nodeIds = do
    maybeSelection <- uses Global.selectionHistory listToMaybe
    let nodeIdsSet = Set.fromList nodeIds
    case maybeSelection of
        Nothing        -> Global.selectionHistory .= [nodeIdsSet]
        Just selection -> when (nodeIdsSet /= selection) $
            Global.selectionHistory %= take historyMaxLength . (nodeIdsSet :)
    when (Set.null nodeIdsSet) dropSelectionHistory
