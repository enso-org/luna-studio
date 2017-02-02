{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Searcher where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command       (Command)
import qualified Luna.Studio.Action.Searcher      as Searcher
import           Luna.Studio.Event.Event          (Event (Shortcut, UI))
import           Luna.Studio.Event.Shortcut       (ShortcutEvent (..))
import           Luna.Studio.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import qualified Luna.Studio.React.Event.App      as App
import qualified Luna.Studio.React.Event.Searcher as Searcher
import           Luna.Studio.State.Action         (Action (continue))
import           Luna.Studio.State.Global         (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut shortcut)                                = Just $ handleShortcut shortcut
handle (UI (AppEvent (App.MouseDown _ _)))                = Just $ continue $ Searcher.close
handle (UI (SearcherEvent (Searcher.InputChanged input))) = Just $ continue $ Searcher.querySearch input
handle _                                                  = Nothing

handleShortcut :: ShortcutEvent -> Command State ()
handleShortcut = \case
    SearcherAccept   -> continue Searcher.accept
    SearcherClose    -> continue Searcher.close
    SearcherMoveDown -> continue Searcher.moveDown
    SearcherMoveUp   -> continue Searcher.moveUp
    SearcherOpen     -> Searcher.open
    _                -> return ()
