{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Shortcut where

import           Data.Aeson          (FromJSON, ToJSON)
import           Luna.Studio.Prelude



data ShortcutEvent = Accept
                   | Cancel
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
                   | Paste Text
                   -- navigation
                   | GoPrev
                   | GoNext
                   | GoLeft
                   | GoUp
                   | GoRight
                   | GoDown
                   | GoConeLeft
                   | GoConeUp
                   | GoConeRight
                   | GoConeDown
                   -- node
                   | SelectAll
                   | RemoveSelectedNodes
                   | UnselectAll
                   | ExpandSelectedNodes
                   -- searcher
                   | SearcherAccept
                   | SearcherClose
                   | SearcherMoveDown
                   | SearcherMoveUp
                   | SearcherOpen
                   -- undo/redo
                   | Undo
                   | Redo
                   deriving (Read, Show, Generic, NFData, Typeable)

instance ToJSON   ShortcutEvent
instance FromJSON ShortcutEvent
