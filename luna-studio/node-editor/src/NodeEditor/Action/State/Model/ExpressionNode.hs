module NodeEditor.Action.State.Model.ExpressionNode where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Monad                              (filterM)
import           LunaStudio.Data.Geometry                   (isPointInCircle, isPointInRectangle)
import           LunaStudio.Data.PortRef                    (AnyPortRef (InPortRef'), toAnyPortRef)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position)
import           LunaStudio.Data.TypeRep                    (TypeRep, matchTypes)
import           NodeEditor.Action.State.Action             (checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor         (getAllNodes, getConnectionsToNode, getExpressionNode, getExpressionNodes,
                                                             getNode, getPort, getScene, modifyExpressionNode, modifyInputNode,
                                                             modifyOutputNode)
import           NodeEditor.React.Model.Connection          (canConnect, dst)
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node                (Node (Expression), portModeAt)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, argumentConstructorRef, countArgPorts, hasPort,
                                                             inPortAt, inPortsList, isCollapsed, isMouseOver, nodeId, nodeLoc, outPortAt,
                                                             outPortsList, position, position, zPos)
import           NodeEditor.React.Model.Port                (AnyPortId (InPortId', OutPortId'), InPortIndex (Arg, Self), Mode (..),
                                                             isOutPort, isSelf, portId, valueType)
import           NodeEditor.State.Action                    (connectSourcePort, penConnectAction)
import           NodeEditor.State.Global                    (State, actions, currentConnectAction)


updateAllPortsMode :: Command State ()
updateAllPortsMode = getAllNodes >>= mapM_ updatePortsModeForNode'

updatePortsModeForNode :: NodeLoc -> Command State ()
updatePortsModeForNode nl = withJustM (getNode nl) updatePortsModeForNode'

updatePortsModeForNode' :: Node -> Command State ()
updatePortsModeForNode' n = do
    mapM_ (updatePortMode' n . OutPortId') . map (view portId) $ outPortsList n
    mapM_ (updatePortMode' n . InPortId')  . map (view portId) $ inPortsList  n
    updateArgConstructorMode' n

updateArgConstructorMode :: NodeLoc -> Command State ()
updateArgConstructorMode nl = withJustM (getNode nl) updateArgConstructorMode'

updateArgConstructorMode' :: Node -> Command State ()
updateArgConstructorMode' n = updatePortMode' n $ InPortId' [Arg $ countArgPorts n]

updatePortMode :: AnyPortRef -> Command State ()
updatePortMode portRef = do
    let nl  = portRef ^. PortRef.nodeLoc
        pid = portRef ^. PortRef.portId
    withJustM (getNode nl) $ flip updatePortMode' pid

updatePortMode' :: Node -> AnyPortId -> Command State ()
updatePortMode' n pid = do
    let nl = n ^. nodeLoc
    portMode <- calculatePortMode n pid
    modifyExpressionNode nl $ portModeAt pid .= portMode
    modifyInputNode      nl $ portModeAt pid .= portMode
    modifyOutputNode     nl $ portModeAt pid .= portMode

calculatePortMode :: Node -> AnyPortId -> Command State Mode
calculatePortMode (Expression node) pid = if isSelf pid then calculatePortSelfMode node else do
    let nl      = node ^. nodeLoc
        portRef = toAnyPortRef nl pid
    penConnecting <- checkIfActionPerfoming penConnectAction
    mayConnectSrc <- view connectSourcePort `fmap2` use (actions . currentConnectAction)
    if      penConnecting && isOutPort pid then return Inactive
    else if penConnecting                  then return Normal
    else if not (hasPort pid node) && isNothing mayConnectSrc then return (if node ^. isMouseOver then Normal else Invisible)
    else flip (maybe (return Normal)) mayConnectSrc $ \connectSrc ->
        if      connectSrc == portRef               then return Highlighted
        else if not (hasPort pid node) && canConnect connectSrc portRef then return Normal
        else if not $ canConnect connectSrc portRef then (return $ if hasPort pid node then Inactive else Invisible)
        else if not $ hasPort pid node              then return Normal
        else do
            mayConnectSrcType <- view valueType `fmap2` getPort connectSrc
            let mayPortValueType :: Maybe TypeRep
                mayPortValueType = case pid of
                    OutPortId' outpid -> node ^? outPortAt outpid . valueType
                    InPortId'  inpid  -> node ^? inPortAt  inpid  . valueType
            return $ case (mayConnectSrcType, mayPortValueType) of
                (Nothing, _)       -> Normal
                (_, Nothing)       -> TypeNotMatched
                (Just t1, Just t2) -> if matchTypes t1 t2 then Normal else TypeNotMatched
calculatePortMode node pid = do
    let nl      = node ^. nodeLoc
        portRef = toAnyPortRef nl pid
    mayConnectSrc <- view connectSourcePort `fmap2` use (actions . currentConnectAction)
    flip (maybe (return Normal)) mayConnectSrc $ \connectSrc ->
        if      connectSrc == portRef               then return Highlighted
        else if not (hasPort pid node) && canConnect connectSrc portRef then return Normal
        else if not $ canConnect connectSrc portRef then return Inactive
        else do
            mayConnectSrcType <- view valueType `fmap2` getPort connectSrc
            let mayPortValueType :: Maybe TypeRep
                mayPortValueType = case pid of
                    OutPortId' outpid -> node ^? outPortAt outpid . valueType
                    InPortId'  inpid  -> node ^? inPortAt  inpid  . valueType
            return $ case (mayConnectSrcType, mayPortValueType) of
                (Nothing, _)       -> Normal
                (_, Nothing)       -> TypeNotMatched
                (Just t1, Just t2) -> if matchTypes t1 t2 then Normal else TypeNotMatched

calculatePortSelfMode :: ExpressionNode -> Command State Mode
calculatePortSelfMode node = do
    let nl           = node ^. nodeLoc
        notCollapsed = not $ isCollapsed node
    penConnecting <- checkIfActionPerfoming penConnectAction
    isConnDst     <- any (isSelf . view (dst . PortRef.dstPortId)) <$> getConnectionsToNode nl
    mayConnectSrc <- view connectSourcePort `fmap2` use (actions . currentConnectAction)
    let (connectToSelfPossible, isConnectSrc) = maybe (False, False)
                                                      ( \src -> ( canConnect src . toAnyPortRef nl $ InPortId' [Self]
                                                                , src ^. PortRef.nodeLoc == nl && isSelf (src ^. PortRef.portId) ) )
                                                      mayConnectSrc
    return $ if isConnectSrc                               then Highlighted
        else if notCollapsed || penConnecting || isConnDst then Normal
        else if connectToSelfPossible                      then TypeNotMatched
                                                           else Invisible

isArgConstructorConnectSrc :: NodeLoc -> Command State Bool
isArgConstructorConnectSrc nl = do
    mayConnectSrc <- view connectSourcePort `fmap2` use (actions . currentConnectAction)
    mayPortRef    <- (InPortRef' . argumentConstructorRef) `fmap2` getExpressionNode nl
    return $ isJust mayConnectSrc && mayConnectSrc == mayPortRef
