{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.Connection
    ( module X
    , module NodeEditor.React.Model.Connection
    ) where

import           Common.Prelude
import           Control.Arrow                              ((&&&))
import           Data.Convert                               (Convertible (convert))
import           Data.HashMap.Strict                        (HashMap)
import qualified Data.HashMap.Strict                        as HashMap
import           LunaStudio.Data.Angle                      (Angle)
import           LunaStudio.Data.Connection                 as X (ConnectionId)
import qualified LunaStudio.Data.Connection                 as Empire
import qualified LunaStudio.Data.LabeledTree                as LT
import           LunaStudio.Data.PortRef                    (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position, move, x, y)
import           LunaStudio.Data.Vector2                    (Vector2 (Vector2))
import           NodeEditor.Data.Color                      (Color)
import           NodeEditor.React.Model.Constants           (lineHeight, nodeExpandedWidth, portAliasRadius, portRadius)
import           NodeEditor.React.Model.Layout              (Layout, inputSidebarPortPosition, outputSidebarPortPosition)
import           NodeEditor.React.Model.Node                (ExpressionNode, Node (Expression), NodeLoc)
import qualified NodeEditor.React.Model.Node                as Node
import           NodeEditor.React.Model.Node.ExpressionNode (countVisibleArgPorts, countVisibleInPorts, countVisibleOutPorts, inPorts,
                                                             isCollapsed, position, visibleArgPortNumber, visibleInPortNumber,
                                                             visibleOutPortNumber)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.React.Model.Port                (EitherPort, InPort, InPortId, IsAlias, IsOnly, IsSelf, OutPort, OutPortId,
                                                             argumentConstructorOffsetY, isSelf, portAngleStart, portAngleStop, portGap,
                                                             portId)
import qualified NodeEditor.React.Model.Port                as Port


data Mode = Normal | Highlighted | Dimmed | Internal deriving (Eq, Show, Typeable, Generic)


data Connection = Connection
        { _rSrc           :: OutPortRef
        , _rDst           :: InPortRef
        , _rIsSidebarConn :: Bool
        , _rMode          :: Mode
        } deriving (Eq, Show, Typeable, Generic)


data PosConnection = PosConnection
        { _pSrc           :: OutPortRef
        , _pDst           :: InPortRef
        , _pSrcPos        :: Position
        , _pDstPos        :: Position
        , _pIsSidebarConn :: Bool
        , _pMode          :: Mode
        , _pColor         :: Color
        } deriving (Eq, Show, Typeable, Generic)


data HalfConnection = HalfConnection
        { _from           :: AnyPortRef
        , _hDst           :: Position
        , _hIsSidebarConn :: Bool
        , _hMode          :: Mode
        } deriving (Eq, Show, Typeable, Generic)

data PosHalfConnection = PosHalfConnection
        { _phSrc           :: Position
        , _phDst           :: Position
        , _phIsSidebarConn :: Bool
        , _phMode          :: Mode
        , _phColor         :: Color
        } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
makeLenses ''PosConnection
makeLenses ''HalfConnection
makeLenses ''PosHalfConnection

class HasSrc a where src :: Lens' a OutPortRef
class HasDst a where
    dst :: Lens' a InPortRef
    connectionId :: Lens' a InPortRef
    connectionId = dst
class HasSrcPos     a where srcPos      :: Lens' a Position
class HasDstPos     a where dstPos      :: Lens' a Position
class IsSidebarConn a where sidebarConn :: Lens' a Bool
class HasMode       a where mode        :: Lens' a Mode
class HasColor      a where color       :: Lens' a Color
instance HasSrc        Connection        where src         = rSrc
instance HasDst        Connection        where dst         = rDst
instance IsSidebarConn Connection        where sidebarConn = rIsSidebarConn
instance HasMode       Connection        where mode        = rMode
instance HasSrc        PosConnection     where src         = pSrc
instance HasDst        PosConnection     where dst         = pDst
instance HasSrcPos     PosConnection     where srcPos      = pSrcPos
instance HasDstPos     PosConnection     where dstPos      = pDstPos
instance IsSidebarConn PosConnection     where sidebarConn = pIsSidebarConn
instance HasMode       PosConnection     where mode        = pMode
instance HasColor      PosConnection     where color       = pColor
instance HasDstPos     HalfConnection    where dstPos      = hDst
instance IsSidebarConn HalfConnection    where sidebarConn = hIsSidebarConn
instance HasMode       HalfConnection    where mode        = hMode
instance HasSrcPos     PosHalfConnection where srcPos      = phSrc
instance HasDstPos     PosHalfConnection where dstPos      = phDst
instance IsSidebarConn PosHalfConnection where sidebarConn = phIsSidebarConn
instance HasMode       PosHalfConnection where mode        = phMode
instance HasColor      PosHalfConnection where color       = phColor

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
toValidEmpireConnection (OutPortRef' src') (InPortRef' dst')     = Just $ Empire.Connection src' dst'
toValidEmpireConnection dst'@(InPortRef' _) src'@(OutPortRef' _) = toValidEmpireConnection src' dst'
toValidEmpireConnection _ _                                      = Nothing

-- This function should be removed once we redesign connections to the same node
toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe Empire.Connection
toValidConnection pr1 pr2 = if pr1 ^. PortRef.nodeLoc == pr2 ^. PortRef.nodeLoc
    then Nothing
    else toValidEmpireConnection pr1 pr2

canConnect :: AnyPortRef -> AnyPortRef -> Bool
canConnect = isJust .: toValidConnection

instance Convertible PosConnection HalfConnection where
    convert = HalfConnection <$> OutPortRef' . view src <*> view dstPos <*> view sidebarConn <*> view mode

toPosConnection :: OutPortRef -> InPortRef -> PosHalfConnection -> PosConnection
toPosConnection src' dst' = PosConnection src' dst' <$> view srcPos <*> view dstPos <*> view sidebarConn <*> view mode <*> view color

instance Convertible Connection Empire.Connection where
    convert = Empire.Connection <$> view src <*> view dst

argConstructorConnectionPos :: ExpressionNode -> Position
argConstructorConnectionPos n = if n ^. ExpressionNode.mode == ExpressionNode.Collapsed
    then n ^. position & y %~ (+ portRadius)
    else move (Vector2 (-nodeExpandedWidth/2) (argumentConstructorOffsetY $ countVisibleInPorts n)) $ n ^. position

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
            srcPortNum = visibleOutPortNumber srcNode $ srcPort ^. portId
            dstArgNum  = visibleArgPortNumber dstNode $ dstPort ^. portId
            dstPortNum = visibleInPortNumber  dstNode $ dstPort ^. portId
            numOfSrcOutPorts = countVisibleOutPorts srcNode
            numOfDstInPorts  = countVisibleInPorts  dstNode
            numOfDstArgPorts = countVisibleArgPorts dstNode
            srcConnPos = connectionSrc  srcPos'
                                        dstPos'
                                        isSrcExp
                                        isDstExp
                                        srcPortNum
                                        numOfSrcOutPorts $ countVisibleOutPorts srcNode + countVisibleArgPorts srcNode == 1
            dstConnPos = connectionDst  srcPos'
                                        dstPos'
                                        isSrcExp
                                        isDstExp
                                        (if isDstExp then dstPortNum      else dstArgNum)
                                        (if isDstExp then numOfDstInPorts else numOfDstArgPorts)
                                        (isSelf $ dstPort ^. portId)
                                        $ has (inPorts . LT.value . Port.state . Port._Connected) dstNode
        return (srcConnPos, dstConnPos)
    _ -> return def

toConnection :: OutPortRef -> InPortRef -> Node -> Node -> Connection
toConnection srcRef dstRef srcNode dstNode = Connection srcRef dstRef sidebarConn' mode' where
    sidebarConn' = has Node._Input srcNode || has Node._Output dstNode
    mode'        = getConnectionMode dstRef dstNode

toHalfConnection :: AnyPortRef -> Node -> Position -> HalfConnection
toHalfConnection portRef n pos = HalfConnection portRef pos sidebarConn' mode' where
    sidebarConn'  = has Node._Input n || has Node._Output n
    mode' = case portRef of
        OutPortRef' {}    -> Normal
        InPortRef' dstRef -> let isPortVisible = elem (dstRef ^. PortRef.dstPortId) . (Node.argumentConstructorRef n ^. PortRef.dstPortId :) . map (view portId) $ Node.inPortsList n in
            if isPortVisible then Normal else Internal

getConnectionMode :: InPortRef -> Node -> Mode
getConnectionMode dstRef dstNode = if isPortVisible then Normal else Internal where
    isPortVisible = elem (dstRef ^. PortRef.dstPortId) . (Node.argumentConstructorRef dstNode ^. PortRef.dstPortId :) . map (view portId) $ Node.inPortsList dstNode


halfConnectionSrcPosition :: Node -> EitherPort -> Position -> Layout -> Maybe Position
halfConnectionSrcPosition (Node.Input  _  ) (Right port) _ layout = inputSidebarPortPosition  port layout
halfConnectionSrcPosition (Node.Output _  ) (Left  port) _ layout = outputSidebarPortPosition port layout
halfConnectionSrcPosition (Expression node) eport mousePos _ =
    Just $ case eport of
        Right port -> connectionSrc pos
                                    mousePos
                                    isExp
                                    False
                                    (visibleOutPortNumber node $ port ^. portId)
                                    numOfSameTypePorts
                                    $ countVisibleOutPorts node + countVisibleArgPorts node == 1
        Left  port -> connectionDst mousePos
                                    pos
                                    False
                                    isExp
                                    (if isExp then visibleInPortNumber node $ port ^. portId else visibleArgPortNumber node $ port ^. portId)
                                    numOfSameTypePorts 
                                    (isSelf $ port ^. portId)
                                    $ has (inPorts . LT.value . Port.state . Port._Connected) node
    where
        pos                = node ^. position
        isExp              = not . isCollapsed $ node
        numOfSameTypePorts = if isLeft eport then if isExp then countVisibleInPorts node else countVisibleArgPorts node else countVisibleOutPorts node



halfConnectionSrcPosition _ _ _ _ = def

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
connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsOnly -> Position
connectionSrc src' dst' isSrcExpanded _isDstExpanded outNum allOuts isSingle =
    if isSrcExpanded then move (Vector2 (nodeExpandedWidth/2) (lineHeight * (fromIntegral outNum))) src'
    else move (Vector2 (portRadius * cos t) (portRadius * sin t)) src' where
        t = if isSingle
            then nodeToNodeAngle src' dst'
            else connectionAngle src' dst' (allOuts - outNum - 1) allOuts

connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSelf -> IsAlias -> Position
connectionDst src' dst' isSrcExpanded isDstExpanded num numOfSameTypePorts isSelf' isAlias = do
    let src''   = if isSrcExpanded then move (Vector2 (nodeExpandedWidth/2) 0) src' else src'
        t       = connectionAngle src'' dst' num numOfSameTypePorts
    if isDstExpanded
        then move (Vector2 (-(nodeExpandedWidth/2)) (lineHeight * (fromIntegral num))) dst'
        else if isSelf'
                then dst'
                else
                    if isAlias then move (Vector2 (portAliasRadius * (-cos t)) (portAliasRadius * (-sin t))) dst'
                               else move (Vector2 (portRadius      * (-cos t)) (portRadius      * (-sin t))) dst'

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
