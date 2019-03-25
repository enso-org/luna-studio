{-# LANGUAGE Strict #-}

module NodeEditor.React.Model.Searcher.UndoRedo where

import Common.Prelude

------------------------
-- === InputState === --
------------------------

-- === Definition === --

data InputState = InputState
    { _text           :: Text
    , _selectionStart :: Int
    , _selectionEnd   :: Int
    } deriving (Show, Eq, Generic)

makeLenses ''InputState

instance NFData  InputState
instance Default InputState where def = InputState mempty 0 0

---------------------------
-- === UndoRedoState === --
---------------------------

-- === Definition === --

data UndoRedoState = UndoRedoState
    { _undoStack :: [InputState]
    , _redoStack :: [InputState]
    , _initial   :: InputState
    } deriving (Show, Eq, Generic)

makeLenses ''UndoRedoState

instance NFData  UndoRedoState
instance Default UndoRedoState where def = UndoRedoState def def def

-- === Public API === --

mk :: InputState -> UndoRedoState
mk = UndoRedoState def def

undo :: UndoRedoState -> UndoRedoState
undo st = case st ^. undoStack of
    []             -> st
    input : inputs -> st & undoStack .~ inputs
                         & redoStack %~ (input :)

redo :: UndoRedoState -> UndoRedoState
redo st = case st ^. redoStack of
    []             -> st
    input : inputs -> st & undoStack %~ (input :)
                         & redoStack .~ inputs

setInput :: InputState -> UndoRedoState -> UndoRedoState
setInput input urSt = urSt & undoStack %~ (input :)
                           & redoStack .~ []

setSelection :: Int -> Int -> UndoRedoState -> UndoRedoState
setSelection start end = set (currentInput . selectionStart) start
                       . set (currentInput . selectionEnd)   end

-- === Internal API === --

currentInput :: Lens' UndoRedoState InputState
currentInput = lens getter setter where
    getter ur = case ur ^. undoStack of
        []          -> ur ^. initial
        (input : _) -> input
    setter ur input = case ur ^. undoStack of
        []      -> ur & initial .~ input
        (_ : _) -> ur & undoStack . ix 0 .~ input

