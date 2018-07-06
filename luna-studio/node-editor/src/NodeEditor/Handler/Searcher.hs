{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Searcher where

import           Common.Prelude

import           Common.Action.Command              (Command)
import           LunaStudio.Data.Position           (fromTuple)
import qualified NodeEditor.Action.Searcher         as Searcher
import           NodeEditor.Action.State.NodeEditor (whenGraphLoaded)
import           NodeEditor.Event.Event             (Event (Shortcut, UI, View))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.Event.UI                (UIEvent (AppEvent, SearcherEvent))
import           NodeEditor.Event.View              (BaseEvent (SearcherAccept, SearcherEdit), SearcherEditEvent (SearcherEditEvent),
                                                     ViewEvent, base)
import qualified NodeEditor.React.Event.App         as App
import qualified NodeEditor.React.Event.Searcher    as Searcher
import           NodeEditor.State.Action            (Action (continue))
import           NodeEditor.State.Global            (State)
import           Text.Read                          (readMaybe)


handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle scheduleEvent (UI (SearcherEvent evt))
    = handleSearcherEvent scheduleEvent evt
handle scheduleEvent (View evt) = handleViewEvent scheduleEvent evt
handle _ (Shortcut evt)         = handleShortcutEvent evt
handle _ (UI (AppEvent evt))    = handleAppEvent evt
handle _ _                      = Nothing


handleAppEvent :: App.Event -> Maybe (Command State ())
handleAppEvent App.ContextMenu    = Just $ whenGraphLoaded $ Searcher.open def
handleAppEvent (App.MouseDown {}) = Just $ continue Searcher.close
handleAppEvent _                  = Nothing

handleShortcutEvent :: Shortcut.ShortcutEvent -> Maybe (Command State ())
handleShortcutEvent evt = case evt ^. Shortcut.shortcut of
    Shortcut.SearcherEditExpression {}
        -> Just $ whenGraphLoaded Searcher.editSelectedNodeExpression
    Shortcut.SearcherOpen
        -> Just . whenGraphLoaded . Searcher.open
            $ fmap fromTuple $ readMaybe =<< evt ^. Shortcut.arg
    _ -> Nothing

handleSearcherEvent
    :: (Event -> IO ()) -> Searcher.Event -> Maybe (Command State ())
handleSearcherEvent scheduleEvent = \case
    Searcher.InputChanged input ss se
        -> Just . continue $ Searcher.updateInput input ss se
    Searcher.Accept
        -> Just . continue $ Searcher.accept scheduleEvent
    Searcher.AcceptInput
        -> Just . continue $ Searcher.acceptWithHint scheduleEvent 0
    Searcher.AcceptWithHint i
        -> Just . continue $ Searcher.acceptWithHint scheduleEvent i
    Searcher.HintShortcut i -> Just . continue $ Searcher.updateInputWithHint i
    Searcher.TabPressed     -> Just $ continue Searcher.handleTabPressed
    Searcher.MoveDown       -> Just $ continue Searcher.selectPreviousHint
    Searcher.MoveUp         -> Just $ continue Searcher.selectNextHint
    _                       -> Nothing

    -- Searcher.KeyUp k                  -> when (Keys.withoutMods k Keys.backspace) $ continue Searcher.enableRollback
    -- Searcher.MoveLeft                 -> continue Searcher.tryRollback

handleViewEvent :: (Event -> IO ()) -> ViewEvent -> Maybe (Command State ())
handleViewEvent scheduleEvent evt = case evt ^. base of
    SearcherAccept {} -> Just . continue $ Searcher.accept scheduleEvent
    SearcherEdit (SearcherEditEvent ss se input)
        -> Just . continue $ Searcher.updateInput input ss se
    _   -> Nothing
