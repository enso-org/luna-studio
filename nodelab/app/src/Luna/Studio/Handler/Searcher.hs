{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Searcher where

import           Luna.Studio.Prelude
import           React.Flux                       (KeyboardEvent)

import           Event.Event                      (Event (UI))
import           Event.UI                         (UIEvent (AppEvent, SearcherEvent))
import           Luna.Studio.Action.Command     (Command)
import qualified Luna.Studio.Action.Searcher    as Searcher
import qualified Luna.Studio.Event.Keys           as Keys
import qualified Luna.Studio.React.Event.App      as App
import qualified Luna.Studio.React.Event.Searcher as Searcher
import           Luna.Studio.State.Global         (State)



toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.MouseDown _))) = Just $ Searcher.close
toAction (UI (AppEvent (App.KeyDown   e))) = Just $ handleAppKey e
toAction (UI (SearcherEvent (Searcher.KeyDown      e    ))) = Just $ handleSearcherKey e
toAction (UI (SearcherEvent (Searcher.InputChanged input))) = Just $ Searcher.querySearch input
toAction _ = Nothing

handleAppKey :: KeyboardEvent -> Command State ()
handleAppKey evt
    | Keys.withoutMods evt Keys.tab = Searcher.open
    | otherwise                     = return ()

handleSearcherKey :: KeyboardEvent -> Command State ()
handleSearcherKey evt
    | Keys.withoutMods evt Keys.enter     = Searcher.accept
    | Keys.withoutMods evt Keys.tab       = Searcher.close
    | Keys.withoutMods evt Keys.esc       = Searcher.close
    | Keys.withoutMods evt Keys.downArrow = Searcher.moveDown
    | Keys.withoutMods evt Keys.upArrow   = Searcher.moveUp
    | otherwise                           = return ()
