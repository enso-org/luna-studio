module NodeEditor.React.Model.Searcher.UndoRedo where

import Common.Prelude

data InputState = InputState
    { _text :: Text
    , _selectionStart :: Int
    , _selectionEnd :: Int
    } deriving (Show, Eq, Generic)

makeLenses ''InputState

instance NFData  InputState
instance Default InputState where def = InputState "" 0 0

data UndoRedoState = UndoRedoState
    { _undoStack :: [InputState]
    , _redoStack :: [InputState]
    , _initial   :: InputState
    } deriving (Show, Eq, Generic)

makeLenses ''UndoRedoState

instance NFData  UndoRedoState
instance Default UndoRedoState where def = UndoRedoState def def def

mk :: InputState -> UndoRedoState
mk = UndoRedoState def def

currentInput :: Lens' UndoRedoState InputState
currentInput = lens getter setter where
    getter ur = case ur ^. undoStack of
        []        -> ur ^. initial
        (inp : _) -> inp
    setter ur inp = case ur ^. undoStack of
        []      -> ur & initial .~ inp
        (_ : _) -> ur & undoStack . ix 0 .~ inp

undo :: UndoRedoState -> UndoRedoState
undo st = case st ^. undoStack of
    []         -> st
    inp : inps -> st & undoStack .~ inps
                     & redoStack %~ (inp :)

redo :: UndoRedoState -> UndoRedoState
redo st = case st ^. redoStack of
    []         -> st
    inp : inps -> st & undoStack %~ (inp :)
                     & redoStack .~ inps

setInput :: InputState -> UndoRedoState -> UndoRedoState
setInput inp urSt = urSt & undoStack %~ (inp :)
                         & redoStack .~ []

setSelection :: Int -> Int -> UndoRedoState -> UndoRedoState
setSelection start end = (set (currentInput . selectionStart) start)
                       . (set (currentInput . selectionEnd)   end)
