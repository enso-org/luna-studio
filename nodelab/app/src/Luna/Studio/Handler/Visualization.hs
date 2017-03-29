{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Visualization where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command            (Command)
import qualified Luna.Studio.Action.Visualization      as Visualization
import           Luna.Studio.Event.Event               (Event (UI))
import qualified Luna.Studio.Event.Mouse               as Mouse
import           Luna.Studio.Event.UI                  (UIEvent (AppEvent, VisualizationEvent))
import qualified Luna.Studio.React.Event.App           as App
import qualified Luna.Studio.React.Event.Visualization as Visualization
import           Luna.Studio.State.Action              (continue)
import           Luna.Studio.State.Global              (State)



handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
    when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
handle (UI (AppEvent  (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
handle (UI (AppEvent  (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
handle _ = Nothing
