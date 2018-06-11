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
import           NodeEditor.Event.View                (BaseEvent (SelectVisualizer), ViewEvent (ViewEvent), base, path, _FocusVisualization, _SelectVisualizer, _ToggleVisualizations)
import qualified NodeEditor.Event.View                as View
import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.Visualization as Visualization
import           NodeEditor.React.Model.Node          (NodeLoc)
import           NodeEditor.React.Model.Visualization (Mode (Preview), Parent (Node), VisualizationId, visualizerId)
import           NodeEditor.State.Action              (continue)
import           NodeEditor.State.Global              (State)


handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent evt))   = Just $ handleVisualizationEvent evt
handle (Shortcut evt)                  = handleShortcutEvent evt
handle (UI (AppEvent (App.Wheel _ _))) = Just exitAnyVisualizationMode
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
    Shortcut.OpenVisualizationPreview
        -> Just $ Visualization.enterVisualizationMode Preview
    Shortcut.CloseVisualizationPreview  -> Just exitAnyVisualizationMode
    _ -> Nothing

unfocusesVisualization :: ViewEvent -> Bool
unfocusesVisualization evt = case evt ^. base of
    View.FocusVisualization {} -> False
    View.Mouse mevt            -> not $ elem (mevt ^. View.type_)
        ["mouseenter", "mouseleave", "mousemove", "mouseout", "mouseover"]
    _                          -> True

handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. path of
    [ "NodeEditor", "NodeVisualization", nlString, visIdString ] -> do
        let parent    = Node $ read nlString
            visId     = read visIdString
            maySelect = Visualization.selectVisualizer parent visId 
                <$> evt ^? base . _SelectVisualizer . View.visualizerId
            mayFocus  = if has (base . _FocusVisualization) evt
                then Just $ Visualization.focusVisualization parent visId
                else Nothing
        listToMaybe $ catMaybes [mayFocus, maySelect]
    _ -> if has (base . _ToggleVisualizations) evt
            then Just . Visualization.toggleVisualizations
                . Node . convert $ evt ^. View.target
        else if unfocusesVisualization evt
            then Just exitAnyVisualizationMode
            else Nothing

exitAnyVisualizationMode :: Command State ()
exitAnyVisualizationMode = do
    continue Visualization.exitVisualizationMode
    continue Visualization.exitDocVisualizationMode

-- handle :: Event -> Maybe (Command State ())
-- handle (UI (VisualizationEvent (Visualization.Pin   nodeLoc visIx         ))) = Just $ Visualization.pin   nodeLoc visIx
-- handle (UI (VisualizationEvent (Visualization.Unpin nodeLoc visIx position))) = Just $ Visualization.unpin nodeLoc visIx position
-- handle (UI (VisualizationEvent (Visualization.MouseDown evt nodeLoc visIx position))) = Just $
--     when (Mouse.withoutMods evt Mouse.leftButton) $ Visualization.startDrag nodeLoc visIx position evt
-- handle (UI (AppEvent (App.MouseMove mevt _))) = Just $ continue $ Visualization.drag mevt
-- handle (UI (AppEvent (App.MouseUp   mevt  ))) = Just $ continue $ Visualization.stopDrag mevt
-- handle _ = Nothing
