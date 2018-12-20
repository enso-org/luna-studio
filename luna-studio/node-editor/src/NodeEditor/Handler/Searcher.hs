module NodeEditor.Handler.Searcher where

import Common.Prelude

import qualified NodeEditor.Event.Shortcut       as Shortcut
import qualified NodeEditor.React.Event.App      as App
import qualified NodeEditor.React.Event.Searcher as Searcher

import Common.Action.Command              (Command)
import LunaStudio.Data.Position           (fromTuple)
import NodeEditor.Action.State.NodeEditor (whenGraphLoaded)
import NodeEditor.Event.Event             (Event (Shortcut, UI))
import NodeEditor.Event.UI                (UIEvent (AppEvent, SearcherEvent))
import NodeEditor.State.Action            (Action (continue))
import NodeEditor.State.Global            (State)
import Text.Read                          (readMaybe)


handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle _ (Shortcut evt) = handleShortcut evt
handle _ (UI       evt) = handleUIEvent  evt
-- handle scheduleEvent (UI (SearcherEvent evt)) = Just $ handleEvent scheduleEvent evt
handle _ _ = Nothing

handleUIEvent :: UIEvent -> Maybe (Command State ())
handleUIEvent (AppEvent App.ContextMenu) 
    = Just . whenGraphLoaded $ Searcher.open def
handleUIEvent (AppEvent (App.MouseDown _ _)) = Just $ continue Searcher.close
handleUIEvent _                              = Nothing

handleShortcut :: Shortcut.Event -> Maybe (Command State ())
handleShortcut (Shortcut.Event Shortcut.SearcherEditExpression _)
    = Just $ whenGraphLoaded Searcher.editSelectedNodeExpression
handleShortcut (Shortcut.Event Shortcut.SearcherOpen arg) = let 
    mayPos = fmap fromTuple $ readMaybe =<< arg
    in Just . whenGraphLoaded $ Searcher.open mayPos
handleShortcut _ = Nothing

-- handleEvent :: (Event -> IO ()) -> Searcher.Event -> Command State ()
-- handleEvent scheduleEvent = \case
--     Searcher.InputChanged input ss se -> continue $ Searcher.updateInput input ss se
--     Searcher.Accept                   -> continue $ Searcher.accept scheduleEvent
--     Searcher.AcceptInput              -> continue $ Searcher.withHint 0 (Searcher.accept scheduleEvent)
--     Searcher.AcceptWithHint i         -> continue $ Searcher.withHint i (Searcher.accept scheduleEvent)
--     Searcher.HintShortcut   i         -> continue $ Searcher.withHint i Searcher.updateInputWithSelectedHint
--     Searcher.TabPressed               -> continue Searcher.handleTabPressed
--     Searcher.MoveDown                 -> continue Searcher.selectPreviousHint
--     -- Searcher.KeyUp k                  -> when (Keys.withoutMods k Keys.backspace) $ continue Searcher.enableRollback
--     -- Searcher.MoveLeft                 -> continue Searcher.tryRollback
--     Searcher.MoveUp                   -> continue Searcher.selectNextHint
--     _                                 -> pure ()
