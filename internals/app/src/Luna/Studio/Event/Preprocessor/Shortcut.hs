module Luna.Studio.Event.Preprocessor.Shortcut
    ( process
    , isEventHandled
    ) where

import           React.Flux                       (KeyboardEvent)

import           Luna.Studio.Event.Event          (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Keys           as Keys
import           Luna.Studio.Event.Shortcut       (Command (..))
import qualified Luna.Studio.Event.Shortcut       as Shortcut
import           Luna.Studio.Event.UI             (UIEvent (AppEvent, SearcherEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App      as App
import qualified Luna.Studio.React.Event.Searcher as Searcher
import qualified React.Flux                       as React


process :: Event -> Maybe Event
process (UI (AppEvent      (App.KeyDown      e))) = Shortcut . flip Shortcut.Event def <$> handleKeyApp e
process (UI (SearcherEvent (Searcher.KeyDown e))) = UI . SearcherEvent <$> handleKeySearcher e
process _ = Nothing

isEventHandled :: KeyboardEvent -> Bool
isEventHandled = isJust . handleKeyApp

handleKeyApp :: KeyboardEvent -> Maybe Command
handleKeyApp evt
    | Keys.withoutMods      evt Keys.esc        = Just Cancel
    -- camera
    | Keys.withCtrl         evt Keys.leftArrow  = Just PanLeft
    | Keys.withCtrl         evt Keys.rightArrow = Just PanRight
    | Keys.withCtrl         evt Keys.upArrow    = Just PanUp
    | Keys.withCtrl         evt Keys.downArrow  = Just PanDown
    | Keys.withCtrl         evt Keys.plus       = Just ZoomIn
    | Keys.withCtrlShift    evt Keys.plus       = Just ZoomIn
    | Keys.withCtrl         evt Keys.minus      = Just ZoomOut
    | Keys.withCtrlShift    evt Keys.minus      = Just ZoomOut
    | Keys.withCtrl         evt Keys.zero       = Just ResetZoom
    | Keys.withCtrlShift    evt Keys.zero       = Just ResetPan
    | Keys.withCtrlAltShift evt Keys.zero       = Just ResetCamera
    | Keys.withCtrlShift    evt Keys.zero       = Just CenterGraph
    | Keys.withoutMods      evt Keys.h          = Just CenterGraph
    -- navigation
    | Keys.withShift        evt Keys.leftArrow  = Just GoPrev
    | Keys.withShift        evt Keys.rightArrow = Just GoNext
    | Keys.withoutMods      evt Keys.leftArrow  = Just GoLeft
    | Keys.withoutMods      evt Keys.upArrow    = Just GoUp
    | Keys.withoutMods      evt Keys.rightArrow = Just GoRight
    | Keys.withoutMods      evt Keys.downArrow  = Just GoDown
    | Keys.withCtrlShift    evt Keys.leftArrow  = Just GoConeLeft
    | Keys.withCtrlShift    evt Keys.upArrow    = Just GoConeUp
    | Keys.withCtrlShift    evt Keys.rightArrow = Just GoConeRight
    | Keys.withCtrlShift    evt Keys.downArrow  = Just GoConeDown
    -- nodes
    | Keys.withCtrl         evt Keys.a          = Just SelectAll
    | Keys.withCtrl         evt Keys.e          = Just UnfoldSelectedNodes
    | Keys.withoutMods      evt Keys.backspace  = Just RemoveSelectedNodes
    | Keys.withoutMods      evt Keys.del        = Just RemoveSelectedNodes
    | Keys.withoutMods      evt Keys.enter      = Just ExpandSelectedNodes
    | Keys.withoutMods      evt Keys.space      = Just EditSelectedNodes
    -- searcher
    | Keys.withoutMods evt Keys.tab             = Just SearcherOpen
    -- undo / redo
    | Keys.withCtrl         evt Keys.z          = Just Undo
    | Keys.withCtrl         evt Keys.y          = Just Redo
    | Keys.withCtrlShift    evt Keys.z          = Just Redo
    --
    | otherwise                                 = Nothing

handleKeySearcher :: KeyboardEvent -> Maybe Searcher.Event
handleKeySearcher evt
    | Keys.withoutMods   evt Keys.backspace = Just   Searcher.MoveLeft
    | Keys.withoutMods   evt Keys.downArrow = Just   Searcher.MoveDown
    | Keys.withoutMods   evt Keys.enter     = Just   Searcher.Accept
    | Keys.withCtrl      evt Keys.enter     = Just   Searcher.AcceptInput
    | Keys.digitWithCtrl evt                = Just $ Searcher.AcceptEntry $ (React.keyCode evt) - Keys.zero
    | Keys.withoutMods   evt Keys.tab       = Just   Searcher.EditEntry
    | Keys.withoutMods   evt Keys.upArrow   = Just   Searcher.MoveUp
    | otherwise                             = Nothing
