{-# LANGUAGE OverloadedStrings #-}

module Node.Editor.Handler.Visualization where

import           Luna.Prelude

import           Node.Editor.Action.Command            (Command)
import qualified Node.Editor.Action.Visualization      as Visualization
import           Node.Editor.Event.Event               (Event (UI))
import qualified Node.Editor.Event.Mouse               as Mouse
import           Node.Editor.Event.UI                  (UIEvent (AppEvent, VisualizationEvent))
import qualified Node.Editor.React.Event.App           as App
import qualified Node.Editor.React.Event.Visualization as Visualization
import           Node.Editor.State.Action              (continue)
import           Node.Editor.State.Global              (State)



handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
    when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
handle (UI (AppEvent (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
handle _ = Nothing
