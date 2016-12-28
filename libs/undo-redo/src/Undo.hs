    UndoMessage :: (Binary undoReq, Binary redoReq) => Topic.Topic -> undoReq -> Topic.Topic -> redoReq -> UndoMessage
