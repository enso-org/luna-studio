module Reactive.Plugins.Core.Action.Collaboration
    ( toAction
    ) where


import           Luna.Studio.Prelude

import qualified Luna.Studio.Batch.Workspace                   as Workspace
import qualified Data.DateTime                     as DT
import qualified Data.Map.Lazy                     as Map

import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Graph.Collaboration    as Collaboration

import           Event.Batch                       (Event (..))
import qualified Event.Event                       as Event

import           Reactive.Commands.Batch           (collaborativeTouch)
import           Reactive.Commands.Collaboration   (updateClient)
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph           (allNodes)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.State.Collaboration      (ColorId)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global

import qualified Object.Widget.Node                as NodeModel
import           React.Store                       (widget)
import qualified React.Store                       as Store



refreshTime, modifyTime :: Integer
refreshTime = 10
modifyTime  =  3

isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

touchCurrentlySelected :: Command State ()
touchCurrentlySelected = do
    selected <- selectedNodes
    let nodeIds = view (widget . NodeModel.nodeId) <$> selected
    collaborativeTouch nodeIds

expireTouchedNodes :: Command State ()
expireTouchedNodes = do
    widgetIds   <- allNodes
    currentTime <- use Global.lastEventTimestamp
    forM_ widgetIds $ Store.modify_ $
        (  NodeModel.collaboration . NodeModel.touch  %~ Map.filter (\(ts, _) -> DT.diffSeconds ts currentTime > 0))
        . (NodeModel.collaboration . NodeModel.modify %~ Map.filter (\ ts     -> DT.diffSeconds ts currentTime > 0))

everyNSeconds :: Integer -> Command State () -> Command State ()
everyNSeconds interval action = do
    currentTime  <- use Global.lastEventTimestamp
    when (DT.toSeconds currentTime `mod` interval == 0) action

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    CollaborationUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. Collaboration.location)
        let clientId = update ^. Collaboration.clientId
            touchNodes nodeIds setter = forM_ nodeIds $ \nodeId -> do
                Global.withNode nodeId $ mapM_ $ Store.modify_ setter
        myClientId   <- use Global.clientId
        currentTime  <- use Global.lastEventTimestamp
        when (shouldProcess && clientId /= myClientId) $ do
            clientColor <- updateClient clientId
            case update ^. Collaboration.event of
                Collaboration.Touch       nodeIds -> touchNodes nodeIds $ NodeModel.collaboration . NodeModel.touch  . at clientId ?~ (DT.addSeconds (2 * refreshTime) currentTime, clientColor)
                Collaboration.Modify      nodeIds -> touchNodes nodeIds $ (NodeModel.collaboration . NodeModel.modify . at clientId ?~ DT.addSeconds modifyTime currentTime)
                                                                        . (NodeModel.collaboration . NodeModel.touch  . at clientId %~ bumpTime (DT.addSeconds modifyTime currentTime) clientColor)
                Collaboration.CancelTouch nodeIds -> touchNodes nodeIds $ NodeModel.collaboration . NodeModel.touch  . at clientId .~ Nothing
                Collaboration.Refresh             -> touchCurrentlySelected

    _ -> return ()

toAction Event.Tick = Just $ do
    expireTouchedNodes
    everyNSeconds refreshTime touchCurrentlySelected

toAction _ = Nothing

bumpTime :: DT.DateTime -> ColorId -> Maybe (DT.DateTime, ColorId) -> Maybe (DT.DateTime, ColorId)
bumpTime time color (Just (time', _)) = Just (max time time', color)
bumpTime time color Nothing           = Just (time, color)
