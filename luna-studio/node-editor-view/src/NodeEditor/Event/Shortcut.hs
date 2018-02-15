{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.Shortcut where

import           Common.Analytics  (IsTrackedEvent)
import           Common.Data.Event (EventName (eventName))
import           Common.Prelude
import           Data.Aeson        (FromJSON)


data Command = Cancel
             | Accept
             | OpenSearcher
             -- camera
             | CenterGraph
             | PanDown
             | PanLeft
             | PanRight
             | PanUp
             | ResetCamera
             | ResetPan
             | ResetZoom
             | ZoomIn
             | ZoomOut
             -- Clipboard
             | Copy
             | Cut
             | Paste
             -- navigation
             | ExitGraph
             | GoConeDown
             | GoConeLeft
             | GoConeRight
             | GoConeUp
             | GoDown
             | GoLeft
             | GoNext
             | GoPrev
             | GoRight
             | GoUp
             -- node
             | AutolayoutAllNodes
             | AutolayoutSelectedNodes
             | CloseVisualizationPreview
             | CollapseToFunction
             | EditSelectedNodes
             | ExpandSelectedNodes
             | OpenVisualizationPreview
             | RemoveSelectedNodes
             | SelectAll
             | UnfoldSelectedNodes
             | ZoomVisualization
             -- searcher
             | SearcherOpen
             | SearcherEditExpression
             -- undo/redo
             | Undo
             | Redo
             -- mock-monads
             | MockAddMonad
             | MockClearMonads
             deriving (Bounded, Eq, Enum, FromJSON, Generic, NFData, Read, Show, Typeable)

data ShortcutEvent = Event
                   { _shortcut :: Command
                   , _arg :: Maybe String
                   } deriving (FromJSON, Generic, NFData, Show, Typeable)

makeLenses ''ShortcutEvent

instance EventName ShortcutEvent where
    eventName = head . words . show . view shortcut

instance IsTrackedEvent ShortcutEvent
