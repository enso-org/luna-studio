{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Searcher where

import           Common.Prelude

import           Common.Action.Command              (Command)
import           LunaStudio.Data.Position           (fromTuple)
import qualified NodeEditor.Action.Searcher         as Searcher
import           NodeEditor.Action.State.NodeEditor (whenGraphLoaded)
import           NodeEditor.Event.Event             (Event (Shortcut, UI))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.Event.UI                (UIEvent (AppEvent, SearcherEvent))
import qualified NodeEditor.React.Event.App         as App
import qualified NodeEditor.React.Event.Searcher    as Searcher
import           NodeEditor.State.Action            (Action (continue))
import           NodeEditor.State.Global            (State)
import           Text.Read                          (readMaybe)

handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle _ (Shortcut (Shortcut.Event Shortcut.SearcherEditExpression _)) = Just $ whenGraphLoaded Searcher.editSelectedNodeExpression
handle _ (Shortcut (Shortcut.Event Shortcut.SearcherOpen         arg)) = Just $ whenGraphLoaded $ Searcher.open $ fmap fromTuple $ readMaybe =<< arg
handle _ (UI (AppEvent App.ContextMenu))                               = Just $ whenGraphLoaded $ Searcher.open def
handle scheduleEvent (UI (SearcherEvent evt))                          = Just $ handleEvent scheduleEvent evt
handle _ (UI (AppEvent (App.MouseDown _ _)))                           = Just $ continue Searcher.close
handle _ _                                                             = Nothing

handleEvent :: (Event -> IO ()) -> Searcher.Event -> Command State ()
handleEvent scheduleEvent = \case
    Searcher.InputChanged input ss se -> continue $ Searcher.updateInput input ss se
    Searcher.Accept                   -> continue $ Searcher.accept scheduleEvent
    Searcher.AcceptInput              -> continue $ Searcher.acceptWithHint scheduleEvent 0
    Searcher.AcceptWithHint i         -> continue $ Searcher.acceptWithHint scheduleEvent i
    Searcher.HintShortcut   i         -> continue $ Searcher.updateInputWithHint i
    Searcher.TabPressed               -> continue Searcher.handleTabPressed
    Searcher.MoveDown                 -> continue Searcher.selectPreviousHint
    -- Searcher.KeyUp k                  -> when (Keys.withoutMods k Keys.backspace) $ continue Searcher.enableRollback
    -- Searcher.MoveLeft                 -> continue Searcher.tryRollback
    Searcher.MoveUp                   -> continue Searcher.selectNextHint
    _                                 -> return ()
