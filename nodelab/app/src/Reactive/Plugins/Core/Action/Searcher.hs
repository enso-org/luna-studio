{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Searcher where

import           React.Flux                 (KeyboardEvent)
import           Luna.Studio.Prelude

import           Event.Event                (Event (UI))
import qualified Event.Keys                 as Keys
import           Event.UI                   (UIEvent (AppEvent, SearcherEvent))
import qualified Luna.Studio.React.Event.App            as App
import qualified Luna.Studio.React.Event.Searcher       as Searcher
import           Reactive.Commands.Command  (Command)
import qualified Reactive.Commands.Searcher as Searcher
import           Luna.Studio.State.Global      (State)



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
