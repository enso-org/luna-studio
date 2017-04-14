module Internal.Handler.App
    ( handle
    ) where

import           Internal.Prelude

import qualified JS.Config                       as Config
import qualified Internal.Action.Batch        as Batch
import           Internal.Action.Command      (Command)
-- import           Internal.Action.State.Action (endAll)
import           Internal.Event.Event         (Event (Init))
-- import           Internal.Event.UI            (UIEvent (AppEvent))
-- import qualified Internal.React.Event.App     as App
import           Internal.State.Global        (State)
import qualified Internal.State.Global        as Global


handle :: Event -> Maybe (Command Global.State ())
-- handle  Init                                 = Just $ maybe Batch.listProjects Batch.openProject Nothing
handle _                                     = Nothing
