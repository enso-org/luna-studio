module Luna.Studio.Handler.MultiSelection
    ( toAction
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


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown evt))) = Just $ when shouldProceed $ startMultiSelection evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent        (App.MouseMove evt)))        = Just $ continue $ updateMultiSelection evt
toAction (UI (AppEvent        (App.MouseUp   _  )))        = Just $ continue $ stopMultiSelection
toAction _                                                 = Nothing
