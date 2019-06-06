{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Internal where

import           Common.Analytics  (IsTrackedEvent (..))
import           Common.Data.Event (EventName)
import           Common.Prelude
import           Data.Aeson        (FromJSON, ToJSON)
import Path (Abs, Dir, File, Path, Rel)

data InternalEvent
    = Copy          { _filePath :: Path Rel File, _selections :: [(Int, Int)]}
    | CloseFile     { _filePath :: Path Rel File }
    | CreateProject { _path :: FilePath }
    | FileChanged   { _path :: FilePath }
    | GetBuffer     { _filePath :: Path Rel File }
    | InterpreterPause
    | InterpreterReload
    | InterpreterStart
    | IsSaved     { _path :: FilePath }
    | MoveProject { _oldPath :: FilePath, _newPath :: FilePath }
    | OpenFile    { _path :: FilePath }
    | Paste       { _selections :: [(Int, Int)], _content :: [Text] }
    | Redo
    | SaveFile    { _filePath :: Path Rel File }
    | SetProject  { _projectPath :: Path Abs Dir }
    | Undo
    deriving (Generic, NFData, Show, Typeable)

makeLenses ''InternalEvent

instance ToJSON   InternalEvent
instance FromJSON InternalEvent

instance EventName InternalEvent
instance IsTrackedEvent InternalEvent
