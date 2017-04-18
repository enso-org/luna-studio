module Internal.Handler.App
    ( handle
    ) where

import           Luna.Prelude

import qualified JS.Config                       as Config
import           Internal.Action.Command      (Command)
import           Internal.Event.Event         (Event)
import           Internal.State.Global        (State)


handle :: Event -> Maybe (Command Global.State ())
handle _                                     = Nothing
