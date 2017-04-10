{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Shortcut where

import           Data.Aeson          (FromJSON, ToJSON)
import           Luna.Studio.Prelude


data Command = Cancel
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
             | SelectAll
             | RemoveSelectedNodes
             | ExpandSelectedNodes
             | UnfoldSelectedNodes
             | EditSelectedNodes
             | AutolayoutSelectedNodes
             | AutolayoutAllNodes
             -- searcher
             | SearcherOpen
             -- undo/redo
             | Undo
             | Redo
             deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data ShortcutEvent = Event
                   { _command :: Command
                   , _arg :: Maybe String
                   } deriving (Generic, NFData, Show, Typeable)

makeLenses ''ShortcutEvent

instance ToJSON   Command
instance FromJSON Command
instance ToJSON   ShortcutEvent
instance FromJSON ShortcutEvent
