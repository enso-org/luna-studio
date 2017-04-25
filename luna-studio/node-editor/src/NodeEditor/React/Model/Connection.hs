module NodeEditor.React.Model.Connection where

import           Common.Prelude
import           Control.Arrow                              ((&&&))
import           Data.Aeson                                 (ToJSON)
import           Data.Convert                               (Convertible (convert))
import           Data.HashMap.Strict                        (HashMap)
import qualified Data.HashMap.Strict                        as HashMap
import           Data.Position                              (Position, move, x, y)
import           Data.Vector2                               (Vector2 (Vector2))
import qualified Empire.API.Data.Connection                 as Empire
import           Empire.API.Data.PortRef                    (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                    as PortRef
import           NodeEditor.Data.Angle                      (Angle)
import           NodeEditor.Data.Color                      (Color)
import           NodeEditor.React.Model.Constants           (gridSize, lineHeight, nodeExpandedWidth, portRadius)
import           NodeEditor.React.Model.Layout              (Layout, inputSidebarPortPosition, outputSidebarPortPosition)
import           NodeEditor.React.Model.Node                (ExpressionNode, Node (Expression), NodeLoc)
import qualified NodeEditor.React.Model.Node                as Node
import           NodeEditor.React.Model.Node.ExpressionNode (countArgPorts, countOutPorts, isCollapsed, position)
import           NodeEditor.React.Model.Port                (EitherPort, InPort, InPortId, OutPort, OutPortId, getPortNumber, isSelf,
                                                             portAngleStart, portAngleStop, portGap, portId)


type ConnectionId = InPortRef
data Mode = Normal | Sidebar | Highlighted | Dimmed deriving (Eq, Show, Typeable, Generic)

instance ToJSON Mode


data Connection = Connection
        { _rSrc  :: OutPortRef
        , _rDst  :: InPortRef
        , _rMode :: Mode
        } deriving (Eq, Show, Typeable, Generic)


data PosConnection = PosConnection
        { _pSrc    :: OutPortRef
        , _pDst    :: InPortRef
        , _pSrcPos  :: Position
        , _pDstPos  :: Position
        , _pMode   :: Mode
        , _pColor  :: Color
        } deriving (Eq, Show, Typeable, Generic)


data HalfConnection = HalfConnection
        { _from  :: AnyPortRef
        , _hDst  :: Position
        , _hMode :: Mode
        } deriving (Eq, Show, Typeable, Generic)

data PosHalfConnection = PosHalfConnection
        { _phSrc    :: Position
        , _phDst    :: Position
        , _phMode   :: Mode
        , _phColor  :: Color
        } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
makeLenses ''PosConnection
makeLenses ''HalfConnection
makeLenses ''PosHalfConnection
instance ToJSON Connection
instance ToJSON PosConnection
instance ToJSON HalfConnection
instance ToJSON PosHalfConnection

class HasSrc a where src :: Lens' a OutPortRef
class HasDst a where
    dst :: Lens' a InPortRef
    connectionId :: Lens' a InPortRef
    connectionId = dst
class HasSrcPos a where srcPos :: Lens' a Position
class HasDstPos a where dstPos :: Lens' a Position
class HasMode   a where mode   :: Lens' a Mode
class HasColor  a where color  :: Lens' a Color
instance HasSrc    Connection        where src    = rSrc
instance HasDst    Connection        where dst    = rDst
instance HasMode   Connection        where mode   = rMode
instance HasSrc    PosConnection     where src    = pSrc
instance HasDst    PosConnection     where dst    = pDst
instance HasSrcPos PosConnection     where srcPos = pSrcPos
instance HasDstPos PosConnection     where dstPos = pDstPos
instance HasMode   PosConnection     where mode   = pMode
instance HasColor  PosConnection     where color  = pColor
instance HasDstPos HalfConnection    where dstPos = hDst
instance HasMode   HalfConnection    where mode   = hMode
instance HasSrcPos PosHalfConnection where srcPos = phSrc
instance HasDstPos PosHalfConnection where dstPos = phDst
instance HasMode   PosHalfConnection where mode   = phMode
instance HasColor  PosHalfConnection where color  = phColor

type ConnectionsMap = HashMap ConnectionId Connection

toConnectionsMap :: [Connection] -> ConnectionsMap
toConnectionsMap = HashMap.fromList . map (view connectionId &&& id)

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


instance Convertible PosConnection HalfConnection where
    convert = HalfConnection <$> OutPortRef' . view src <*> view dstPos <*> view mode

toPosConnection :: OutPortRef -> InPortRef -> PosHalfConnection -> PosConnection
toPosConnection src' dst' = PosConnection src' dst' <$> view srcPos <*> view dstPos <*> view mode <*> view color

instance Convertible Connection Empire.Connection where
    convert = Empire.Connection <$> view src <*> view dst

portPhantomPosition :: ExpressionNode -> Position
portPhantomPosition n = n ^. position & y %~ (+ 2 * gridSize)

connectionPositions :: Node -> OutPort -> Node -> InPort -> Layout -> Maybe (Position, Position)
connectionPositions srcNode' srcPort dstNode' dstPort layout = case (srcNode', dstNode') of
    (Node.Input _, Node.Output _) -> do
        srcConnPos <- inputSidebarPortPosition srcPort layout
        dstConnPos <- outputSidebarPortPosition dstPort layout
        return (srcConnPos, dstConnPos)
    (Node.Input _, _) -> do
        srcConnPos <- inputSidebarPortPosition srcPort layout
        dstConnPos <- halfConnectionSrcPosition dstNode' (Left dstPort) srcConnPos layout
        return (srcConnPos, dstConnPos)
    (_, Node.Output _) -> do
        dstConnPos <- outputSidebarPortPosition dstPort layout
        srcConnPos <- halfConnectionSrcPosition srcNode' (Right srcPort) dstConnPos layout
        return (srcConnPos, dstConnPos)
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
        return (srcConnPos, dstConnPos)

connectionMode :: Node -> Node -> Mode
connectionMode (Node.Input {}) _  = Sidebar
connectionMode _ (Node.Output {}) = Sidebar
connectionMode _ _ = Normal

halfConnectionMode :: Node -> Mode
halfConnectionMode (Node.Expression {}) = Sidebar
halfConnectionMode _                    = Normal

halfConnectionSrcPosition :: Node -> EitherPort -> Position -> Layout -> Maybe Position
halfConnectionSrcPosition (Node.Input  _  ) (Right port) _ layout = inputSidebarPortPosition  port layout
halfConnectionSrcPosition (Node.Output _  ) (Left  port) _ layout = outputSidebarPortPosition port layout
halfConnectionSrcPosition (Expression node) eport mousePos _ = Just $ case eport of
        Right port -> connectionSrc pos mousePos isExp False (getPortNumber $ port ^. portId) numOfSameTypePorts $ countOutPorts node + countArgPorts node == 1
        Left  port -> connectionDst mousePos pos False isExp (getPortNumber $ port ^. portId) numOfSameTypePorts $ isSelf $ port ^. portId
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
        t = if isSingle
            then nodeToNodeAngle src' dst'
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
    if src' == dst' then pi else 
        if srcX < dstX
            then atan ((srcY - dstY) / (srcX - dstX))
            else atan ((srcY - dstY) / (srcX - dstX)) + pi
    where
        srcX = src' ^. x
        srcY = src' ^. y
        dstX = dst' ^. x
        dstY = dst' ^. y
