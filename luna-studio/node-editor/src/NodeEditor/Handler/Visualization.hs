{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Visualization where

import           Common.Prelude

import           NodeEditor.Action.Command            (Command)
import qualified NodeEditor.Action.Visualization      as Visualization
import           NodeEditor.Event.Event               (Event (Shortcut, UI))
import qualified NodeEditor.Event.Mouse               as Mouse
import qualified NodeEditor.Event.Shortcut            as Shortcut
import           NodeEditor.Event.UI                  (UIEvent (AppEvent, NodeEditorEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.NodeEditor    as NodeEditor
import qualified NodeEditor.React.Event.Visualization as Visualization
import           NodeEditor.State.Action              (continue, end)
import           NodeEditor.State.Action              (VisualizationActive)
import           NodeEditor.State.Global              (State)


handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Focus               nl)))         = Just $ Visualization.focusVisualization nl
handle (UI (VisualizationEvent (Visualization.SelectVisualization nl visName))) = Just $ Visualization.selectVisualization nl visName
handle (Shortcut (Shortcut.Event Shortcut.ZoomVisualization _))                 = Just $ Visualization.zoomVisualization
handle (UI (NodeEditorEvent    (NodeEditor.Wheel _ _)))                         = Just $ continue $ Visualization.closeVisualization
handle _                                                                        = Nothing

-- handle :: Event -> Maybe (Command State ())
-- handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
-- handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
-- handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
--     when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
-- handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
-- handle (UI (AppEvent (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
-- handle _ = Nothing
