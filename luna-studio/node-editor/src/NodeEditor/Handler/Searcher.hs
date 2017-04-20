{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Searcher where

import           Common.Prelude

import           NodeEditor.Action.Command         (Command)
import qualified NodeEditor.Action.Searcher        as Searcher
import           NodeEditor.Event.Event            (Event (Shortcut, UI))
import qualified NodeEditor.Event.Keys             as Keys
import qualified NodeEditor.Event.Shortcut         as Shortcut
import           NodeEditor.Event.UI               (UIEvent (AppEvent, NodeEditorEvent, SearcherEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified NodeEditor.React.Event.NodeEditor as NodeEditor
import qualified NodeEditor.React.Event.Searcher   as Searcher
import           NodeEditor.State.Action           (Action (continue))
import           NodeEditor.State.Global           (State)


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
