{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Shortcut where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.List           as List
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
             | ExpandSelectedNodes
             -- searcher
             | SearcherAccept
             | SearcherMoveDown
             | SearcherMoveUp
             | SearcherOpen
             | SearcherMoveRight
             | SearcherMoveLeft
             -- undo/redo
             | Undo
             | Redo
             deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data ShortcutEvent = Event
                   { _command :: Command
                   , _arg :: Maybe Text
                   } deriving (Generic, NFData, Show, Typeable)

makeLenses ''ShortcutEvent

instance ToJSON   Command
instance FromJSON Command
instance ToJSON   ShortcutEvent
instance FromJSON ShortcutEvent

fromString :: String -> ShortcutEvent
fromString str = result where
    (commandStr, argStr) = List.break (== ' ') str & _2 %~ drop 1
    result = Event (read commandStr) $ if null argStr then Nothing else Just $ convert argStr
