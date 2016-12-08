{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Searcher where

import           React.Flux                                 (KeyboardEvent, keyCode)
import           Utils.PreludePlus

import           Event.Event                                (Event (UI))
import qualified Event.Keys                                 as Keys
import           Event.UI                                   (UIEvent (AppEvent, SearcherEvent))
import qualified React.Event.App                            as App
import qualified React.Event.Searcher                       as Searcher
import           Reactive.Commands.Command                  (Command)
import qualified Reactive.Commands.Searcher                 as Searcher
import           Reactive.State.Global                      (State)



toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.MouseDown _))) = Just $ Searcher.close
toAction (UI (AppEvent (App.KeyDown   e))) = Just $ handleAppKey e
toAction (UI (SearcherEvent (Searcher.KeyDown      e    ))) = Just $ handleSearcherKey e
toAction (UI (SearcherEvent (Searcher.InputChanged input))) = Just $ print input
toAction _ = Nothing

handleAppKey :: KeyboardEvent -> Command State ()
handleAppKey evt
    | keyCode evt == Keys.tab = Searcher.open
    | otherwise               = print evt

handleSearcherKey :: KeyboardEvent -> Command State ()
handleSearcherKey evt
    | keyCode evt == Keys.esc = Searcher.close
    | otherwise               = print evt
