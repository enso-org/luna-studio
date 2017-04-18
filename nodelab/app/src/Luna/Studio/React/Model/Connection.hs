module Luna.Studio.React.Model.Connection where

import           Control.Arrow                               ((&&&))
import           Data.Aeson                                  (ToJSON)
import           Data.Convert                                (Convertible (convert))
import           Data.HashMap.Strict                         (HashMap)
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position, move, x, y)
import           Data.Vector2                                 (Vector2 (Vector2))
import qualified Empire.API.Data.Connection                  as Empire
import           Empire.API.Data.PortRef                     (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                     as PortRef
import           Luna.Studio.Data.Angle                      (Angle)
import           Luna.Studio.Data.Color                      (Color)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants           (gridSize, lineHeight, nodeExpandedWidth, portRadius)
import           Luna.Studio.React.Model.Layout              (Layout, inputSidebarPortPosition, outputSidebarPortPosition)
import           Luna.Studio.React.Model.Node                (ExpressionNode, Node (Expression), NodeLoc)
import qualified Luna.Studio.React.Model.Node                as Node
import           Luna.Studio.React.Model.Node.ExpressionNode (countArgPorts, countOutPorts, isCollapsed, position)
import           Luna.Studio.React.Model.Port                (EitherPort, InPort, InPortId, OutPort, OutPortId, getPortNumber, isSelf,
                                                              portAngleStart, portAngleStop, portGap, portId)


type ConnectionId = InPortRef
data Mode = Normal | Sidebar | Highlighted | Dimmed deriving (Eq, Show, Typeable, Generic)

instance ToJSON Mode

data Connection = Connection { _src    :: OutPortRef
                             , _dst    :: InPortRef
                             , _srcPos :: Position
                             , _dstPos :: Position
                             , _mode   :: Mode
                             , _color  :: Color
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

data CurrentConnection = CurrentConnection { _currentFrom  :: Position
                                           , _currentTo    :: Position
                                           , _currentMode  :: Mode
                                           , _currentColor :: Color
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection


type ConnectionsMap = HashMap ConnectionId Connection

toConnectionsMap :: [Connection] -> ConnectionsMap
toConnectionsMap = HashMap.fromList . map (view connectionId &&& id)


connectionId :: Lens' Connection ConnectionId
connectionId = dst

srcNodeLoc :: Lens' Connection NodeLoc
srcNodeLoc = src . PortRef.srcNodeLoc

srcPortId :: Lens' Connection OutPortId
srcPortId = src . PortRef.srcPortId

dstNodeLoc :: Lens' Connection NodeLoc
dstNodeLoc = dst . PortRef.dstNodeLoc

dstPortId :: Lens' Connection InPortId
dstPortId = dst . PortRef.dstPortId

raw :: Getter Connection (OutPortRef, InPortRef)
raw = to raw' where
    raw' conn = (conn ^. src, conn ^. dst)

nodeLocs :: Getter Connection (NodeLoc, NodeLoc)
nodeLocs = to nodeLocs' where
    nodeLocs' conn = ( conn ^. src . PortRef.srcNodeLoc
                    , conn ^. dst . PortRef.dstNodeLoc )

containsNode :: NodeLoc -> Connection -> Bool
containsNode nid conn = (conn ^. srcNodeLoc == nid)
                     || (conn ^. dstNodeLoc == nid)

containsPortRef :: AnyPortRef -> Connection -> Bool
containsPortRef (InPortRef'  inPortRef)  conn = conn ^. dst == inPortRef
containsPortRef (OutPortRef' outPortRef) conn = conn ^. src == outPortRef

toValidEmpireConnection :: AnyPortRef -> AnyPortRef -> Maybe Empire.Connection
toValidEmpireConnection (OutPortRef' src') (InPortRef' dst')     =
    if src' ^. PortRef.srcNodeLoc /= dst' ^. PortRef.dstNodeLoc
    then Just $ Empire.Connection src' dst'
    else Nothing
toValidEmpireConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidEmpireConnection src' dst'
toValidEmpireConnection _ _                                      = Nothing


instance Convertible Connection CurrentConnection where
    convert = CurrentConnection <$> view srcPos <*> view dstPos <*> view mode <*> view color

toConnection :: OutPortRef -> InPortRef -> CurrentConnection -> Connection
toConnection src' dst' = Connection src' dst' <$> view currentFrom <*> view currentTo <*> view currentMode <*> view currentColor

instance Convertible Connection Empire.Connection where
    convert = Empire.Connection <$> view src <*> view dst

portPhantomPosition :: ExpressionNode -> Position
portPhantomPosition n = n ^. position & y %~ (+ 2 * gridSize)

connectionPositionAndMode :: Node -> OutPort -> Node -> InPort -> Layout -> Maybe ((Position, Position), Mode)
connectionPositionAndMode srcNode' srcPort dstNode' dstPort layout = case (srcNode', dstNode') of
    (Node.Input _, Node.Output _) -> do
        srcConnPos <- inputSidebarPortPosition srcPort layout
        dstConnPos <- outputSidebarPortPosition dstPort layout
        return ((srcConnPos, dstConnPos), Sidebar)
    (Node.Input _, _) -> do
        srcConnPos <- inputSidebarPortPosition srcPort layout
        dstConnPos <- fst <$> currentConnectionSrcPositionAndMode dstNode' (Left dstPort) srcConnPos layout
        return ((srcConnPos, dstConnPos), Sidebar)
    (_, Node.Output _) -> do
        dstConnPos <- outputSidebarPortPosition dstPort layout
        srcConnPos <- fst <$> currentConnectionSrcPositionAndMode srcNode' (Right srcPort) dstConnPos layout
        return ((srcConnPos, dstConnPos), Sidebar)
    (Expression srcNode, Expression dstNode) -> do
        let srcPos'    = srcNode ^. position
            dstPos'    = dstNode ^. position
            isSrcExp   = not . isCollapsed $ srcNode
            isDstExp   = not . isCollapsed $ dstNode
            srcPortNum = getPortNumber $ srcPort ^. portId
            dstPortNum = getPortNumber $ dstPort ^. portId
            numOfSrcOutPorts = countOutPorts srcNode
            numOfDstInPorts  = countArgPorts dstNode
            srcConnPos = connectionSrc srcPos' dstPos' isSrcExp isDstExp srcPortNum numOfSrcOutPorts $ countOutPorts srcNode + countArgPorts srcNode == 1
            dstConnPos = connectionDst srcPos' dstPos' isSrcExp isDstExp dstPortNum numOfDstInPorts $ isSelf $ dstPort ^. portId
        return ((srcConnPos, dstConnPos), Normal)

currentConnectionSrcPositionAndMode :: Node -> EitherPort -> Position -> Layout -> Maybe (Position, Mode)
currentConnectionSrcPositionAndMode (Node.Input  _  ) (Right port) _ layout = (, Sidebar) <$> inputSidebarPortPosition  port layout
currentConnectionSrcPositionAndMode (Node.Output _  ) (Left  port) _ layout = (, Sidebar) <$> outputSidebarPortPosition port layout
currentConnectionSrcPositionAndMode (Expression node) eport mousePos _ = Just $ case eport of
        Right port -> (, Normal) $ connectionSrc pos mousePos isExp False (getPortNumber $ port ^. portId) numOfSameTypePorts $ countOutPorts node + countArgPorts node == 1
        Left  port -> (, Normal) $ connectionDst mousePos pos False isExp (getPortNumber $ port ^. portId) numOfSameTypePorts $ isSelf $ port ^. portId
    where
        pos                = node ^. position
        isExp              = not . isCollapsed $ node
        numOfSameTypePorts = case eport of
            Right _ -> countOutPorts node
            Left  _ -> countArgPorts node

connectionAngle :: Position -> Position -> Int -> Int -> Double
connectionAngle srcPos' dstPos' num numOfSameTypePorts =
    if      t' > a' - pi / 2 - g then a - pi / 2 - g
    else if t' < b' - pi / 2 + g then b - pi / 2 + g
    else t where
        a  = portAngleStop  True num numOfSameTypePorts portRadius
        b  = portAngleStart True num numOfSameTypePorts portRadius
        t  = nodeToNodeAngle srcPos' dstPos'
        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t
        g  = portGap portRadius / 4

-- TODO[JK]: dst numOfInputs
connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionSrc src' dst' isSrcExpanded _isDstExpanded num numOfSameTypePorts isSingle =
    if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src'
    else move (Vector2 (portRadius * cos t) (portRadius * sin t)) src' where
        t = if isSingle then
                 nodeToNodeAngle src' dst'
            else connectionAngle src' dst' num numOfSameTypePorts

connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionDst src' dst' isSrcExpanded isDstExpanded num numOfSameTypePorts isSelf' =
    if isSelf'
        then dst'
    else if isDstExpanded
        then move (Vector2 0 (lineHeight * (fromIntegral num + 1))) dst'
        else move (Vector2 (portRadius * (-cos t)) (portRadius * (-sin t))) dst' where
            src'' = if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src' else src'
            t    = connectionAngle src'' dst' num numOfSameTypePorts


nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src' dst' =
    let srcX = src' ^. x
        srcY = src' ^. y
        dstX = dst' ^. x
        dstY = dst' ^. y
    in  if srcX < dstX
            then atan ((srcY - dstY) / (srcX - dstX))
            else atan ((srcY - dstY) / (srcX - dstX)) + pi
