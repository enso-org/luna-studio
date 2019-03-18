module NodeEditor.Event.Preprocessor.Shortcut where

import           Common.Prelude
import           NodeEditor.Event.Event          (Event (Shortcut, UI))
import           NodeEditor.Event.KeyMap         (handleKeyApp)
import qualified NodeEditor.Event.Shortcut       as Shortcut
import           NodeEditor.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import qualified NodeEditor.React.Event.App      as App
import qualified NodeEditor.React.Event.Searcher as Searcher


process :: Event -> Maybe Event
process (UI (AppEvent      (App.KeyDown      e))) = Shortcut . flip Shortcut.Event def <$> handleKeyApp e
process _ = Nothing
