module Luna.Studio.Handler.App
    ( handle
    ) where

import           Luna.Studio.Prelude

import qualified JS.Config                       as Config
import qualified Luna.Studio.Action.Batch        as Batch
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.State.Action (endAll)
import           Luna.Studio.Event.Event         (Event (Init, UI))
import           Luna.Studio.Event.UI            (UIEvent (AppEvent))
import qualified Luna.Studio.React.Event.App     as App
import           Luna.Studio.State.Global        (State)
import qualified Luna.Studio.State.Global        as Global


handle :: Event -> Maybe (Command Global.State ())
-- handle  Init                                 = Just $ maybe Batch.listProjects Batch.openProject Nothing
handle _                                     = Nothing
