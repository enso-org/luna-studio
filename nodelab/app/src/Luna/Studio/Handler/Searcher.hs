{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Searcher where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command       (Command)
import qualified Luna.Studio.Action.Searcher      as Searcher
import           Luna.Studio.Event.Event          (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Shortcut       as Shortcut
import           Luna.Studio.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import qualified Luna.Studio.React.Event.App      as App
import qualified Luna.Studio.React.Event.Searcher as Searcher
import           Luna.Studio.State.Action         (Action (continue))
import           Luna.Studio.State.Global         (State)


handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle scheduleEvent (Shortcut (Shortcut.Event command _))  = Just $ handleCommand scheduleEvent command
handle _ (UI (AppEvent (App.MouseDown _ _)))                = Just $ continue   Searcher.close
handle _ (UI (SearcherEvent (Searcher.InputChanged input))) = Just $ continue $ Searcher.querySearch input
handle _  _                                                 = Nothing

handleCommand :: (Event -> IO ()) -> Shortcut.Command -> Command State ()
handleCommand scheduleEvent = \case
    Shortcut.SearcherAccept    -> continue $ Searcher.accept scheduleEvent
    Shortcut.SearcherClose     -> continue Searcher.close
    Shortcut.SearcherMoveDown  -> continue Searcher.moveDown
    Shortcut.SearcherMoveLeft  -> continue Searcher.rollback
    Shortcut.SearcherMoveRight -> continue $ Searcher.proceed scheduleEvent
    Shortcut.SearcherMoveUp    -> continue Searcher.moveUp
    Shortcut.SearcherOpen      -> Searcher.open
    _                          -> return ()
