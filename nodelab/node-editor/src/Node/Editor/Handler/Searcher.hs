{-# LANGUAGE OverloadedStrings #-}

module Node.Editor.Handler.Searcher where

import           Luna.Prelude

import           Node.Editor.Action.Command         (Command)
import qualified Node.Editor.Action.Searcher        as Searcher
import           Node.Editor.Event.Event            (Event (Shortcut, UI))
import qualified Node.Editor.Event.Keys             as Keys
import qualified Node.Editor.Event.Shortcut         as Shortcut
import           Node.Editor.Event.UI               (UIEvent (AppEvent, NodeEditorEvent, SearcherEvent))
import qualified Node.Editor.React.Event.App        as App
import qualified Node.Editor.React.Event.NodeEditor as NodeEditor
import qualified Node.Editor.React.Event.Searcher   as Searcher
import           Node.Editor.State.Action           (Action (continue))
import           Node.Editor.State.Global           (State)


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
    Searcher.EditEntry          -> continue $ Searcher.substituteInputWithEntry
    Searcher.MoveDown           -> continue Searcher.moveDown
    Searcher.KeyUp k            -> when (Keys.withoutMods k Keys.backspace) $
                                      continue Searcher.enableRollback
    Searcher.MoveLeft           -> continue Searcher.tryRollback
    Searcher.MoveUp             -> continue Searcher.moveUp
    _                           -> return ()
