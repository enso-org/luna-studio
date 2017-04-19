{-# LANGUAGE TypeApplications #-}
module Node.Editor.Action.Port.Highlight
    ( handleMouseEnter
    , handleMouseLeave
    ) where

import qualified Data.Set                                    as Set
import           Empire.API.Data.Port                        (AnyPortId (InPortId', OutPortId'), InPortIndex (Self))
import           Empire.API.Data.PortRef                     (AnyPortRef, nodeLoc, portId)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.Connect                  ()
import           Luna.Prelude
import           Node.Editor.React.Model.Connection          (toValidEmpireConnection)
import           Node.Editor.React.Model.Node.ExpressionNode (inPortAt, isCollapsed, outPortAt)
import           Node.Editor.React.Model.Port                (Mode (Highlighted, Normal), mode)
import           Node.Editor.State.Action                    (actionsBlockingPortHighlight, connectAction, connectSourcePort)

import           Node.Editor.Action.State.Action             (checkAction, runningActions)
import           Node.Editor.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNode)
import           Node.Editor.State.Global                    (State)


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
