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
handle (UI (VisualizationEvent (Visualization.Pin   nodeId visIx))) = Just $ Visualization.pin   nodeId visIx
handle (UI (VisualizationEvent (Visualization.Unpin nodeId visIx))) = Just $ Visualization.unpin nodeId visIx
handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeId visIx))) = Just $
    when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeId visIx evt
handle (UI (AppEvent  (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
handle (UI (AppEvent  (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
handle _ = Nothing
