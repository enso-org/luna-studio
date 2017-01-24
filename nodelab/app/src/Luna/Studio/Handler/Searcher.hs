{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Searcher where

import           Luna.Studio.Prelude
import           React.Flux                       (KeyboardEvent)

import           Luna.Studio.Action.Command       (Command)
import qualified Luna.Studio.Action.Searcher      as Searcher
import           Luna.Studio.Event.Event          (Event (UI))
import qualified Luna.Studio.Event.Keys           as Keys
import           Luna.Studio.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import qualified Luna.Studio.React.Event.App      as App
import qualified Luna.Studio.React.Event.Searcher as Searcher
import           Luna.Studio.State.Action         (Action (continue))
import           Luna.Studio.State.Global         (State)


toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.MouseDown _))) = Just $ continue Searcher.close
toAction (UI (AppEvent (App.KeyDown   e))) = Just $ handleAppKey e
toAction (UI (SearcherEvent (Searcher.KeyDown      e    ))) = Just $ handleSearcherKey e
toAction (UI (SearcherEvent (Searcher.InputChanged input))) = Just $ continue $ Searcher.querySearch input
toAction _ = Nothing

handleAppKey :: KeyboardEvent -> Command State ()
handleAppKey evt
    | Keys.withoutMods evt Keys.tab = Searcher.open
    | otherwise                     = return ()

handleSearcherKey :: KeyboardEvent -> Command State ()
handleSearcherKey evt
    | Keys.withoutMods evt Keys.enter     = continue $ Searcher.accept
    | Keys.withoutMods evt Keys.tab       = continue $ Searcher.close
    | Keys.withoutMods evt Keys.esc       = continue $ Searcher.close
    | Keys.withoutMods evt Keys.downArrow = continue $ Searcher.moveDown
    | Keys.withoutMods evt Keys.upArrow   = continue $ Searcher.moveUp
    | otherwise                           = return ()
