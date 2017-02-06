module Luna.Studio.Handler.MultiSelection
    ( handle
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.MultiSelection  (startMultiSelection, stopMultiSelection, updateMultiSelection)
import           Luna.Studio.Event.Event            (Event (UI))
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (NodeEditorEvent (NodeEditor.MouseDown evt)))   = Just $ when shouldProceed $ startMultiSelection evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
handle (UI (AppEvent        (App.MouseMove evt _)))        = Just $ continue $ updateMultiSelection evt
handle (UI (AppEvent        (App.MouseUp   _)))            = Just $ continue   stopMultiSelection
handle _                                                   = Nothing
