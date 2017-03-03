{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Searcher where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Action.Searcher        as Searcher
import           Luna.Studio.Event.Event            (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Shortcut         as Shortcut
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, NodeEditorEvent, SearcherEvent))
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Event.Searcher   as Searcher
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)


handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle _ (Shortcut (Shortcut.Event Shortcut.SearcherOpen _)) = Just   Searcher.open
handle _ (UI (NodeEditorEvent NodeEditor.ContextMenu))       = Just   Searcher.open
handle scheduleEvent (UI (SearcherEvent evt))                = Just $ handleEvent scheduleEvent evt
handle _ (UI (AppEvent (App.MouseDown _ _)))                 = Just $ continue Searcher.close
handle _ _                                                   = Nothing

handleEvent :: (Event -> IO ()) -> Searcher.Event -> Command State ()
handleEvent scheduleEvent = \case
    Searcher.InputChanged input -> continue $ Searcher.querySearch input
    Searcher.Accept             -> continue $ Searcher.accept scheduleEvent
    Searcher.AcceptInput        -> continue $ Searcher.acceptEntry scheduleEvent 0
    Searcher.AcceptEntry  i     -> continue $ Searcher.acceptEntry scheduleEvent i
    Searcher.MoveDown           -> continue Searcher.moveDown
    Searcher.MoveLeft           -> continue Searcher.rollback
    Searcher.MoveRight          -> continue $ Searcher.proceed scheduleEvent
    Searcher.MoveUp             -> continue Searcher.moveUp
    _                           -> return ()
