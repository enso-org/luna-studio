module Node.Editor.Handler.MultiSelection
    ( handle
    ) where

import           Node.Editor.Action.Command         (Command)
import           Node.Editor.Action.MultiSelection  (startMultiSelection, stopMultiSelection, updateMultiSelection)
import           Node.Editor.Event.Event            (Event (UI))
import qualified Node.Editor.Event.Mouse            as Mouse
import           Node.Editor.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.App        as App
import qualified Node.Editor.React.Event.NodeEditor as NodeEditor
import           Node.Editor.State.Action           (Action (continue))
import           Node.Editor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (NodeEditorEvent (NodeEditor.MouseDown evt)))   = Just $ when shouldProceed $ startMultiSelection evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
handle (UI (AppEvent        (App.MouseMove evt _)))        = Just $ continue $ updateMultiSelection evt
handle (UI (AppEvent        (App.MouseUp   _)))            = Just $ continue   stopMultiSelection
handle _                                                   = Nothing
