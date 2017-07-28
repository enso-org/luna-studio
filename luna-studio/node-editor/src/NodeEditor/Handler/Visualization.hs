{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Handler.Visualization where

import           Common.Action.Command                (Command)
import           Common.Prelude
import qualified NodeEditor.Action.Visualization      as Visualization
import           NodeEditor.Event.Event               (Event (Shortcut, UI))
import qualified NodeEditor.Event.Shortcut            as Shortcut
import           NodeEditor.Event.UI                  (UIEvent (NodeEditorEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.NodeEditor    as NodeEditor
import qualified NodeEditor.React.Event.Visualization as Visualization
import           NodeEditor.React.Model.Visualization (VisualizationMode (Preview))
import           NodeEditor.State.Action              (continue)
import           NodeEditor.State.Global              (State)


handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Focus            nl visId)))         = Just $ Visualization.focusVisualization nl visId
handle (UI (VisualizationEvent (Visualization.SelectVisualizer nl visId visName))) = Just $ Visualization.selectVisualizer nl visId visName
handle (UI (VisualizationEvent (Visualization.ToggleVisualizations nl)))           = Just $ Visualization.toggleVisualizations nl
handle (Shortcut (Shortcut.Event Shortcut.ZoomVisualization _))                    = Just $ Visualization.handleZoomVisualization
handle (Shortcut (Shortcut.Event Shortcut.OpenVisualizationPreview _))             = Just $ Visualization.enterVisualizationMode Preview
handle (Shortcut (Shortcut.Event Shortcut.CloseVisualizationPreview _))            = Just $ continue $ Visualization.exitPreviewMode
handle (UI (NodeEditorEvent    (NodeEditor.Wheel _ _)))                            = Just $ continue $ Visualization.exitVisualizationMode
handle _                                                                           = Nothing

-- handle :: Event -> Maybe (Command State ())
-- handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
-- handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
-- handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
--     when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
-- handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
-- handle (UI (AppEvent (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
-- handle _ = Nothing
