module Reactive.Commands.Graph.SelectionHistory where

import qualified Data.Set                  as Set
import           Utils.PreludePlus

import           Empire.API.Data.Node      (NodeId)

import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global


historyMaxLength = 10

dropSelectionHistory :: Command State ()
dropSelectionHistory = Global.selectionHistory .= def

modifySelectionHistory :: [NodeId] -> Command State ()
modifySelectionHistory nodeIds = do
    maybeSelection <- uses Global.selectionHistory listToMaybe
    let nodeIdsSet = Set.fromList nodeIds
    case maybeSelection of
        Nothing        -> Global.selectionHistory .= [nodeIdsSet]
        Just selection -> when (nodeIdsSet /= selection) $ do
            Global.selectionHistory %= (take historyMaxLength) . (nodeIdsSet :)
    when (Set.null nodeIdsSet) dropSelectionHistory
