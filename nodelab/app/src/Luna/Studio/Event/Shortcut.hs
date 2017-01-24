{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Shortcut where

import           Control.DeepSeq     (NFData)
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
                   deriving (Read, Show, Generic, NFData, Typeable)

instance ToJSON   ShortcutEvent
instance FromJSON ShortcutEvent
