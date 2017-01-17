module Luna.Studio.Handler.MultiSelection
    ( toAction
    ) where

import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.MultiSelection  (endMultiSelection, startMultiSelection, updateMultiSelection)
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import           Luna.Studio.State.StatefulAction   (StatefulAction (continue))


toAction :: Event -> Maybe (Command State ())
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown evt))) = Just $ when shouldProceed $ startMultiSelection evt where
    shouldProceed = Mouse.withoutMods evt Mouse.leftButton
toAction (UI (AppEvent        (App.MouseMove evt)))        = Just $ continue $ updateMultiSelection evt
toAction (UI (AppEvent        (App.MouseUp   _  )))        = Just $ continue $ endMultiSelection
toAction _                                                 = Nothing
