module NodeEditor.Event.KeyMap where

import           React.Flux                      (KeyboardEvent)

import           Common.Prelude
import qualified NodeEditor.Event.Keys           as Keys
import           NodeEditor.Event.Shortcut       (Command (..))
import qualified React.Flux                      as React


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
    | Keys.withCtrl         evt Keys.esc        = Just ExitGraph
    | Keys.withCtrlShift    evt Keys.downArrow  = Just GoConeDown
    | Keys.withCtrlShift    evt Keys.leftArrow  = Just GoConeLeft
    | Keys.withCtrlShift    evt Keys.rightArrow = Just GoConeRight
    | Keys.withCtrlShift    evt Keys.upArrow    = Just GoConeUp
    | Keys.withoutMods      evt Keys.downArrow  = Just GoDown
    | Keys.withoutMods      evt Keys.leftArrow  = Just GoLeft
    | Keys.withoutMods      evt Keys.rightArrow = Just GoRight
    | Keys.withoutMods      evt Keys.upArrow    = Just GoUp
    | Keys.withShift        evt Keys.leftArrow  = Just GoPrev
    | Keys.withShift        evt Keys.rightArrow = Just GoNext
    -- nodes
    | Keys.withCtrl         evt Keys.a          = Just SelectAll
    | Keys.withCtrl         evt Keys.e          = Just UnfoldSelectedNodes
    | Keys.withCtrl         evt Keys.enter      = Just EditSelectedNodes
    | Keys.withCtrl         evt Keys.l          = Just AutolayoutAllNodes
    | Keys.withCtrl         evt Keys.space      = Just ZoomVisualization
    | Keys.withoutMods      evt Keys.backspace  = Just RemoveSelectedNodes
    | Keys.withoutMods      evt Keys.del        = Just RemoveSelectedNodes
    | Keys.withoutMods      evt Keys.enter      = Just ExpandSelectedNodes
    | Keys.withoutMods      evt Keys.f          = Just CollapseToFunction
    | Keys.withoutMods      evt Keys.l          = Just AutolayoutSelectedNodes
    -- searcher
    | Keys.withoutMods evt Keys.tab             = Just SearcherOpen
    | Keys.withShift   evt Keys.tab             = Just SearcherEditExpression

    -- undo / redo
    | Keys.withCtrl         evt Keys.z          = Just Undo
    | Keys.withCtrl         evt Keys.y          = Just Redo
    | Keys.withCtrlShift    evt Keys.z          = Just Redo
    -- mock-monads
    | Keys.withCtrl         evt Keys.m          = Just MockAddMonad
    | Keys.withCtrlShift    evt Keys.m          = Just MockClearMonads
    --
    | otherwise                                 = Nothing
