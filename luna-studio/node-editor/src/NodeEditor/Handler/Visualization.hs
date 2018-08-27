{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Handler.Visualization where

import           Common.Action.Command                (Command)
import           Common.Prelude
import qualified NodeEditor.Action.Visualization      as Visualization
import           NodeEditor.Event.Event               (Event (Shortcut, UI))
import           NodeEditor.Event.Event               (Event (View))
import           NodeEditor.Event.Shortcut            (ShortcutEvent)
import qualified NodeEditor.Event.Shortcut            as Shortcut
import           NodeEditor.Event.UI                  (UIEvent (AppEvent, VisualizationEvent))
import           NodeEditor.Event.View                (ViewEvent, base, path)
import qualified NodeEditor.Event.View                as View
import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.Visualization as Visualization
import           NodeEditor.React.Model.Visualization (Parent (Node))
import           NodeEditor.State.Global              (State)


handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent evt))   = Just $ handleVisualizationEvent evt
handle (Shortcut evt)                  = handleShortcutEvent evt
handle (UI (AppEvent (App.Wheel _ _))) = Just Visualization.exitAnyVisualizationMode
handle (View evt)                      = handleViewEvent evt
handle _                               = Nothing

handleVisualizationEvent :: Visualization.Event -> Command State ()
handleVisualizationEvent visEvt = let
        visParent = visEvt ^. Visualization.visParent
    in case visEvt ^. Visualization.evtType of
        Visualization.Focus visId
            -> Visualization.focusVisualization visParent visId
        Visualization.SelectVisualizer visId visName
            -> Visualization.selectVisualizer visParent visId visName
        Visualization.ToggleVisualizations
            -> Visualization.toggleVisualizations visParent

handleShortcutEvent :: ShortcutEvent -> Maybe (Command State())
handleShortcutEvent evt = case evt ^. Shortcut.shortcut of
    Shortcut.ZoomVisualization -> Just $ Visualization.handleZoomVisualization
    Shortcut.OpenVisualizationPreview  -> Just $ Visualization.openPreview
    Shortcut.CloseVisualizationPreview -> Just $ Visualization.closePreview
    _ -> Nothing

unfocusesVisualization :: ViewEvent -> Bool
unfocusesVisualization evt = case evt ^. base of
    View.FocusVisualization {} -> False
    View.Mouse mevt            -> not $ elem (mevt ^. View.type_)
        ["mouseenter", "mouseleave", "mousemove", "mouseout", "mouseover"]
    _                          -> True

handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. base of
    View.FocusVisualization evt' -> Just $ do
        let parent = Node $ View.getNodeLoc evt
            visualizationId = View.getVisualizationId evt
        Visualization.focusVisualization parent visualizationId
    View.SelectVisualizer evt' -> Just $ do
        let parent = Node $ View.getNodeLoc evt
            visualizerId = evt' ^. View.visualizerId
            visualizationId = View.getVisualizationId evt
        Visualization.selectVisualizer parent visualizationId visualizerId
    View.ToggleVisualizations _ -> Just . Visualization.toggleVisualizations
        . Node $ View.getNodeLoc evt
    _ -> if unfocusesVisualization evt
            then Just Visualization.exitAnyVisualizationMode
            else Nothing
