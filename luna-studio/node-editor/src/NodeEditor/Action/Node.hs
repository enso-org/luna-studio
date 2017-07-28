{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Node
    ( handleNodeDragMouseUp
    , nodesDrag
    , startNodeDrag
    , editExpression
    , editName
    , snap
    , snapCoord
    , setMouseOver
    , unsetMouseOver
    , showFullError
    ) where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           LunaStudio.Data.Geometry                   (snap, snapCoord)
import           NodeEditor.Action.NodeDrag                 (handleNodeDragMouseUp, nodesDrag, startNodeDrag)
import           NodeEditor.Action.Searcher                 (editExpression)
import           NodeEditor.Action.Searcher                 (editName)
import           NodeEditor.Action.State.NodeEditor         (modifyExpressionNode)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode (isErrorExpanded, isMouseOver)
import           NodeEditor.State.Global                    (State)


setMouseOver :: NodeLoc -> Command State ()
setMouseOver   nl = modifyExpressionNode nl $ isMouseOver .= True

unsetMouseOver :: NodeLoc -> Command State ()
unsetMouseOver nl = modifyExpressionNode nl $ isMouseOver .= False

showFullError :: NodeLoc -> Command State ()
showFullError  nl = modifyExpressionNode nl $ isErrorExpanded .= True
