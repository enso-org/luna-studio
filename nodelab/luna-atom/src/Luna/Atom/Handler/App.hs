module Luna.Atom.Handler.App
    ( handle
    ) where

import           Luna.Prelude

import qualified JS.Config                       as Config
import           Luna.Atom.Action.Command      (Command)
import           Luna.Atom.Event.Event         (Event)
import           Luna.Atom.State.Global        (State)


handle :: Event -> Maybe (Command Global.State ())
handle _                                     = Nothing
