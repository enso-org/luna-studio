module NodeEditor.Handler.Searcher where

import Common.Prelude

import qualified NodeEditor.Action.Searcher      as Searcher
import qualified NodeEditor.Event.Shortcut       as Shortcut
import qualified NodeEditor.React.Event.App      as App
import qualified NodeEditor.React.Event.Searcher as Searcher

import Common.Action.Command              (Command)
import Luna.Benchmark                     (timeAction)
import LunaStudio.Data.Position           (fromTuple)
import NodeEditor.Action.State.NodeEditor (whenGraphLoaded)
import NodeEditor.Event.Event             (Event (Shortcut, UI))
import NodeEditor.Event.UI                (UIEvent (AppEvent, SearcherEvent))
import NodeEditor.State.Action            (Action (continue))
import NodeEditor.State.Global            (State)
import Text.Read                          (readMaybe)


handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle _ (Shortcut (Shortcut.Event Shortcut.SearcherEditExpression _))
    = Just $ timeAction "Shortcut.SearcherEditExpression"
           $ whenGraphLoaded Searcher.editSelectedNodeExpression
handle _ (Shortcut (Shortcut.Event Shortcut.SearcherOpen arg))
    = Just $ timeAction "Shortcut.SearcherOpen"
           $ whenGraphLoaded $ Searcher.open $ fmap fromTuple
                                             $ readMaybe =<< arg
handle _ (UI (AppEvent App.ContextMenu))
    = Just $ whenGraphLoaded $ Searcher.open def
handle scheduleEvent (UI (SearcherEvent evt))
    = Just $ timeAction "SearcherEvent" $ handleEvent scheduleEvent evt
handle _ (UI (AppEvent (App.MouseDown _ _)))
    = Just $ continue Searcher.close
handle _ _ = Nothing

handleEvent :: (Event -> IO ()) -> Searcher.Event -> Command State ()
handleEvent scheduleEvent = \case
    Searcher.InputChanged input ss se -> continue $ Searcher.modifyInput input ss se
    Searcher.Accept                   -> continue $ Searcher.accept scheduleEvent
    Searcher.AcceptInput              -> continue $ Searcher.withHint 0 (Searcher.accept scheduleEvent)
    Searcher.AcceptWithHint i         -> continue $ Searcher.withHint i (Searcher.accept scheduleEvent)
    Searcher.HintShortcut   i         -> continue $ Searcher.withHint i Searcher.updateInputWithSelectedHint
    Searcher.TabPressed               -> continue Searcher.handleTabPressed
    Searcher.MoveDown                 -> continue Searcher.selectPreviousHint
    -- Searcher.KeyUp k                  -> when (Keys.withoutMods k Keys.backspace) $ continue Searcher.enableRollback
    -- Searcher.MoveLeft                 -> continue Searcher.tryRollback
    Searcher.MoveUp                   -> continue Searcher.selectNextHint
    Searcher.ParenOpen                -> continue $ Searcher.openParen
    Searcher.ParenClose               -> continue $ Searcher.closeParen
    Searcher.Backspace                -> continue $ Searcher.backspace
    Searcher.Undo                     -> continue $ Searcher.undo
    Searcher.Redo                     -> continue $ Searcher.redo
    Searcher.SelectionChanged         -> continue $ Searcher.updateSelection
    _                                 -> pure ()
