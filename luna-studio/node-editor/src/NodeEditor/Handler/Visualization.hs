{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Visualization where

import           Common.Prelude

import           NodeEditor.Action.Command            (Command)
import qualified NodeEditor.Action.Visualization      as Visualization
import           NodeEditor.Event.Event               (Event (UI))
import qualified NodeEditor.Event.Mouse               as Mouse
import           NodeEditor.Event.UI                  (UIEvent (AppEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.Visualization as Visualization
import           NodeEditor.State.Action              (continue)
import           NodeEditor.State.Global              (State)



handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
    when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
handle (UI (AppEvent (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
handle _ = Nothing
