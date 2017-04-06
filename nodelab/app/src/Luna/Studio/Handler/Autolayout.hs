module Luna.Studio.Handler.Autolayout
    ( handle
    ) where

import           Luna.Studio.Action.Autolayout (autolayoutAllNodes, autolayoutSelectedNodes)
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.Event.Event       (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut    as Shortcut
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global      (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Autolayout    _)) = Just autolayoutSelectedNodes
handle (Shortcut (Shortcut.Event Shortcut.AutolayoutAll _)) = Just autolayoutAllNodes
handle _                                                    = Nothing
