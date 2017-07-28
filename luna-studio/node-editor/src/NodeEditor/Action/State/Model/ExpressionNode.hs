module NodeEditor.Action.State.Model.ExpressionNode where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Monad                              (filterM)
import qualified JS.Node                                    as JS
import           LunaStudio.Data.Geometry                   (isPointInCircle, isPointInRectangle)
import           LunaStudio.Data.PortRef                    (AnyPortRef, toAnyPortRef)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position)
import           LunaStudio.Data.TypeRep                    (TypeRep, matchTypes)
import           NodeEditor.Action.State.Action             (checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor         (getConnectionsToNode, getExpressionNode, getExpressionNodes, getPort, getScene,
                                                             modifyExpressionNode)
import           NodeEditor.React.Model.Connection          (canConnect, dst)
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, argConstructorMode, countArgPorts, hasPort, inPortAt,
                                                             inPortsList, isCollapsed, nodeId, nodeLoc, outPortAt, outPortsList, position,
                                                             position, zPos)
import           NodeEditor.React.Model.Port                (AnyPortId (InPortId', OutPortId'), InPortIndex (Arg, Self), Mode (..),
                                                             isOutPort, isSelf, mode, portId, valueType)
import           NodeEditor.State.Action                    (connectSourcePort, penConnectAction)
import           NodeEditor.State.Global                    (State, actions, currentConnectAction)


isPointInNode :: Position -> ExpressionNode -> Command State Bool
isPointInNode p node =
    if isCollapsed node
        then return $ isPointInCircle p (node ^. position, nodeRadius)
        else do
            let nid = node ^. nodeId
            getScene >>= \case
                Just scene -> isPointInRectangle p <$> JS.expandedNodeRectangle scene nid
                Nothing -> return False

getNodeAtPosition :: Position -> Command State (Maybe NodeLoc)
getNodeAtPosition p = do
    nodes <- getExpressionNodes >>= filterM (isPointInNode p)
    if null nodes
        then return Nothing
        else return $ Just $ maximumBy (\node1 node2 -> compare (node1 ^. zPos) (node2 ^. zPos)) nodes ^. nodeLoc

updateAllPortsMode :: Command State ()
updateAllPortsMode = getExpressionNodes >>= mapM_ updatePortsModeForNode'

updatePortsModeForNode :: NodeLoc -> Command State ()
updatePortsModeForNode nl = withJustM (getExpressionNode nl) $ updatePortsModeForNode'

updatePortsModeForNode' :: ExpressionNode -> Command State ()
updatePortsModeForNode' n = do
    mapM_ (updatePortMode' n . OutPortId') . map (view portId) $ outPortsList n
    mapM_ (updatePortMode' n . InPortId')  . map (view portId) $ inPortsList  n
    updatePortMode' n $ InPortId' [Arg $ countArgPorts n]


updatePortMode :: AnyPortRef -> Command State ()
updatePortMode portRef = do
    let nl  = portRef ^. PortRef.nodeLoc
        pid = portRef ^. PortRef.portId
    withJustM (getExpressionNode nl) $ flip updatePortMode' pid

updatePortMode' :: ExpressionNode -> AnyPortId -> Command State ()
updatePortMode' n pid = do
    let nl = n ^. nodeLoc
    portMode <- calculatePortMode n pid
    modifyExpressionNode nl $ if not $ hasPort pid n
        then argConstructorMode .= portMode
        else case pid of
            InPortId'  inpid  -> inPortAt  inpid  . mode .= portMode
            OutPortId' outpid -> outPortAt outpid . mode .= portMode

calculatePortMode :: ExpressionNode -> AnyPortId -> Command State Mode
calculatePortMode node pid = if isSelf pid then calculatePortSelfMode node else do
    let nl      = node ^. nodeLoc
        portRef = toAnyPortRef nl pid
    penConnecting <- checkIfActionPerfoming penConnectAction
    mayConnectSrc <- view connectSourcePort `fmap2` use (actions . currentConnectAction)
    if      penConnecting && isOutPort pid then return Inactive
    else if penConnecting                  then return Normal
    else flip (maybe (return Normal)) mayConnectSrc $ \connectSrc ->
        if      connectSrc == portRef               then return Highlighted
        else if not $ canConnect connectSrc portRef then return Inactive
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
    return $ if notCollapsed || penConnecting || isConnDst then Normal
        else if connectToSelfPossible                      then TypeNotMatched
        else if isConnectSrc                               then Highlighted
                                                           else Invisible
