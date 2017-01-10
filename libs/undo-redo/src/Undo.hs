instance Eq UndoMessage where
    (UndoMessage _ reqID1 _ _ _ _) == (UndoMessage _ reqID2 _ _ _ _) = (reqID1 == reqID2)
