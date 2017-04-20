module NodeEditor.Handler.MultiSelection
    ( handle
    ) where

import           NodeEditor.Action.Command         (Command)
import           NodeEditor.Action.MultiSelection  (startMultiSelection, stopMultiSelection, updateMultiSelection)
import           NodeEditor.Event.Event            (Event (UI))
import qualified NodeEditor.Event.Mouse            as Mouse
import           NodeEditor.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Common.Prelude
import qualified NodeEditor.React.Event.App        as App
import qualified NodeEditor.React.Event.NodeEditor as NodeEditor
import           NodeEditor.State.Action           (Action (continue))
import           NodeEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (NodeEditorEvent (NodeEditor.MouseDown evt)))   = Just $ when shouldProceed $ startMultiSelection evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
handle (UI (AppEvent        (App.MouseMove evt _)))        = Just $ continue $ updateMultiSelection evt
handle (UI (AppEvent        (App.MouseUp   _)))            = Just $ continue   stopMultiSelection
handle _                                                   = Nothing
