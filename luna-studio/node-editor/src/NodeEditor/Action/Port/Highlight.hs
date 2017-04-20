{-# LANGUAGE TypeApplications #-}
module NodeEditor.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import qualified Data.Set                                    as Set
import           Empire.API.Data.Port                        (AnyPortId (InPortId', OutPortId'), InPortIndex (Self))
import           Empire.API.Data.PortRef                     (AnyPortRef, nodeLoc, portId)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.Connect                  ()
import           Common.Prelude
import           NodeEditor.React.Model.Connection          (toValidEmpireConnection)
import           NodeEditor.React.Model.Node.ExpressionNode (inPortAt, isCollapsed, outPortAt)
import           NodeEditor.React.Model.Port                (Mode (Highlighted, Normal), mode)
import           NodeEditor.State.Action                    (actionsBlockingPortHighlight, connectAction, connectSourcePort)

import           NodeEditor.Action.State.Action             (checkAction, runningActions)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode)
import           NodeEditor.State.Global                    (State)


handleMouseEnter :: AnyPortRef -> Command State ()
handleMouseEnter portRef = do
    let nl     = portRef ^. nodeLoc
    let anyPid = portRef ^. portId
    mayNode <- getExpressionNode nl
    withJust mayNode $ \node -> do
        mayConnectAction <- checkAction connectAction
        case (view connectSourcePort <$> mayConnectAction) of
            Just src -> when (isJust $ toValidEmpireConnection src portRef) $
                modifyExpressionNode nl $ case anyPid of
                    OutPortId' pid -> outPortAt pid . mode .= Highlighted
                    InPortId'  pid -> inPortAt  pid . mode .= Highlighted

            Nothing  -> do
                actions <- Set.fromList <$> runningActions
                let notBlocked = Set.null (Set.intersection actions actionsBlockingPortHighlight)
                    highlight' = notBlocked && (anyPid /= InPortId' [Self] || (not . isCollapsed $ node))
                    updateMode mode' = case (highlight', mode') of
                        (True,  _)           -> Highlighted
                        (False, Highlighted) -> Normal
                        _                    -> mode'
                modifyExpressionNode nl $ case anyPid of
                    OutPortId' pid -> outPortAt pid . mode %= updateMode
                    InPortId'  pid -> inPortAt  pid . mode %= updateMode

handleMouseLeave :: AnyPortRef -> Command State ()
handleMouseLeave portRef = modifyExpressionNode (portRef ^. nodeLoc) $ case portRef ^. portId of
    OutPortId' pid -> outPortAt pid . mode .= Normal
    InPortId'  pid -> inPortAt  pid . mode .= Normal
