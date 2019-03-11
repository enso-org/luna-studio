{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.Connection
    ( module X
    , module NodeEditor.React.Model.Connection
    ) where

import           Common.Prelude

import qualified Data.HashMap.Strict                        as HashMap
import qualified LunaStudio.Data.Connection                 as Empire
import qualified LunaStudio.Data.LabeledTree                as LT
import qualified LunaStudio.Data.PortRef                    as PortRef
import qualified NodeEditor.React.Model.Node                as Node
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.Port                as Port

import Control.Arrow                              ((&&&))
import Data.Convert                               (Convertible (convert))
import Data.HashMap.Strict                        (HashMap)
import LunaStudio.Data.Angle                      (Angle)
import LunaStudio.Data.Connection                 as X (ConnectionId)
import LunaStudio.Data.Geometry                   (Radius)
import LunaStudio.Data.PortRef                    (AnyPortRef (InPortRef', OutPortRef'),
                                                   InPortRef, OutPortRef)
import LunaStudio.Data.Position                   (Position, move, x, y)
import LunaStudio.Data.Vector2                    (Vector2 (Vector2))
import NodeEditor.Data.Color                      (Color)
import NodeEditor.React.Model.Constants           (lineHeight,
                                                   nodeExpandedWidth,
                                                   portAliasRadius, portRadius)
import NodeEditor.React.Model.Layout              (Layout,
                                                   inputSidebarPortPosition,
                                                   outputSidebarPortPosition)
import NodeEditor.React.Model.Node                (ExpressionNode,
                                                   Node (Expression), NodeLoc)
import NodeEditor.React.Model.Node.ExpressionNode (countVisibleArgPorts,
                                                   countVisibleInPorts,
                                                   countVisibleOutPorts,
                                                   inPorts, isCollapsed,
                                                   position,
                                                   visibleArgPortNumber,
                                                   visibleInPortNumber,
                                                   visibleOutPortNumber)
import NodeEditor.React.Model.Port                (EitherPort, InPort, InPortId,
                                                   IsAlias, IsOnly, IsSelf,
                                                   OutPort, OutPortId,
                                                   argumentConstructorNumber,
                                                   isSelf, portAngleStart,
                                                   portAngleStop, portId)


data Mode
    = Normal
    | Highlighted
    | Dimmed
    | Internal
    deriving (Eq, Show, Typeable, Generic)


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
toValidEmpireConnection (OutPortRef' src') (InPortRef' dst')
    = Just $ Empire.Connection src' dst'
toValidEmpireConnection dst'@(InPortRef' _) src'@(OutPortRef' _)
    = toValidEmpireConnection src' dst'
toValidEmpireConnection _ _ = Nothing

-- This function should be removed once we redesign connections to the same node
toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe Empire.Connection
toValidConnection pr1 pr2 = if pr1 ^. PortRef.nodeLoc == pr2 ^. PortRef.nodeLoc
    then Nothing
    else toValidEmpireConnection pr1 pr2

canConnect :: AnyPortRef -> AnyPortRef -> Bool
canConnect = isJust .: toValidConnection

instance Convertible PosConnection HalfConnection where
    convert = HalfConnection <$> OutPortRef'
        .   view src
        <*> view dstPos
        <*> view sidebarConn
        <*> view mode

toPosConnection :: OutPortRef -> InPortRef -> PosHalfConnection -> PosConnection
toPosConnection src' dst' = PosConnection src' dst'
    <$> view srcPos
    <*> view dstPos
    <*> view sidebarConn
    <*> view mode
    <*> view color

instance Convertible Connection Empire.Connection where
    convert = Empire.Connection <$> view src <*> view dst

argConstructorPosition :: ExpressionNode -> Position
argConstructorPosition n =
    if n ^. ExpressionNode.mode == ExpressionNode.Collapsed
        then n ^. position & y %~ (+ portRadius)
        else expandedInputPosition (n ^. position)
            $ argumentConstructorNumber $ countVisibleInPorts n


connectionPositions :: Node -> OutPort -> Node -> InPort -> Layout
    -> Maybe (Position, Position)
connectionPositions srcNode' srcPort dstNode' dstPort layout
    = case (srcNode', dstNode') of
        (Node.Input _, Node.Output _) -> do
            srcConnPos <- inputSidebarPortPosition srcPort layout
            dstConnPos <- outputSidebarPortPosition dstPort layout
            return (srcConnPos, dstConnPos)
        (Node.Input _, _) -> do
            srcConnPos <- inputSidebarPortPosition srcPort layout
            dstConnPos <- halfConnectionSrcPosition
                dstNode'
                (Left dstPort)
                srcConnPos
                layout
            return (srcConnPos, dstConnPos)
        (_, Node.Output _) -> do
            dstConnPos <- outputSidebarPortPosition dstPort layout
            srcConnPos <- halfConnectionSrcPosition
                srcNode'
                (Right srcPort)
                dstConnPos
                layout
            return (srcConnPos, dstConnPos)

        (Expression srcNode, Expression dstNode) -> do
            let srcPos'    = srcNode ^. position
                dstPos'    = dstNode ^. position

                isSrcExp   = not . isCollapsed $ srcNode
                isDstExp   = not . isCollapsed $ dstNode

                srcPortNum = visibleOutPortNumber srcNode $ srcPort ^. portId
                dstArgNum  = visibleArgPortNumber dstNode $ dstPort ^. portId
                dstPortNum = visibleInPortNumber  dstNode $ dstPort ^. portId

                srcArgs    = countVisibleArgPorts srcNode
                srcPorts   = countVisibleOutPorts srcNode
                dstPorts   = countVisibleInPorts  dstNode
                dstArgs    = countVisibleArgPorts dstNode

                srcConnPos = connectionSrc
                    srcPos'
                    dstPos'
                    isSrcExp
                    isDstExp
                    srcPortNum
                    srcPorts
                    (srcPorts + srcArgs == 1)
                    (if isDstExp then dstPortNum else dstArgNum)
                    (if isDstExp then dstPorts   else dstArgs)

                dstConnPos = connectionDst
                    srcPos'
                    dstPos'
                    isSrcExp
                    isDstExp
                    (if isDstExp then dstPortNum else dstArgNum)
                    (if isDstExp then dstPorts   else dstArgs)
                    (isSelf $ dstPort ^. portId)
                    (has
                        (inPorts . LT.value . Port.state . Port._Connected)
                        dstNode
                    )
                    srcPortNum
                    srcPorts
            return (srcConnPos, dstConnPos)
        _ -> return def

toConnection :: OutPortRef -> InPortRef -> Node -> Node -> Connection
toConnection srcRef dstRef srcNode dstNode
    = Connection srcRef dstRef sidebarConn' mode' where
        sidebarConn' = has Node._Input srcNode || has Node._Output dstNode
        mode'        = getConnectionMode dstRef dstNode

toHalfConnection :: AnyPortRef -> Node -> Position -> HalfConnection
toHalfConnection portRef n pos
    = HalfConnection portRef pos sidebarConn' mode' where
        sidebarConn'  = has Node._Input n || has Node._Output n
        mode' = case portRef of
            OutPortRef' {}    -> Normal
            InPortRef' dstRef
                -> let isPortVisible = elem (dstRef ^. PortRef.dstPortId)
                        . (Node.argumentConstructorRef n ^. PortRef.dstPortId :)
                        $ (view portId) <$> Node.inPortsList n
                    in if isPortVisible then Normal else Internal

getConnectionMode :: InPortRef -> Node -> Mode
getConnectionMode dstRef dstNode = if isPortVisible
    then Normal
    else Internal where
        isPortVisible = elem
            (dstRef ^. PortRef.dstPortId)
                . (Node.argumentConstructorRef dstNode ^. PortRef.dstPortId :)
                    $ (view portId) <$> Node.inPortsList dstNode


halfConnectionSrcPosition :: Node -> EitherPort -> Position -> Layout
    -> Maybe Position
halfConnectionSrcPosition (Node.Input  _  ) (Right port) _ layout
    = inputSidebarPortPosition  port layout
halfConnectionSrcPosition (Node.Output _  ) (Left  port) _ layout
    = outputSidebarPortPosition port layout
halfConnectionSrcPosition (Expression node) eport mousePos _ =
    Just $ case eport of
        Right port -> connectionSrc
            pos
            mousePos
            isExp
            False
            (visibleOutPortNumber node $ port ^. portId)
            allPorts
            (countVisibleOutPorts node + countVisibleArgPorts node == 1)
            1
            1
        Left port -> connectionDst
            mousePos
            pos
            False
            isExp
            (if isExp
                then visibleInPortNumber node $ port ^. portId
                else visibleArgPortNumber node $ port ^. portId
            )
            allPorts
            (isSelf $ port ^. portId)
            (has (inPorts . LT.value . Port.state . Port._Connected) node)
            1
            1
    where
        pos      = node ^. position
        isExp    = not . isCollapsed $ node
        allPorts = if isLeft eport
            then if isExp
                then countVisibleInPorts node
                else countVisibleArgPorts node
            else countVisibleOutPorts node

halfConnectionSrcPosition _ _ _ _ = def


connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsOnly
    -> Int -> Int -> Position
connectionSrc srcNode dstNode srcExpanded _dstExpanded srcPortNum srcPorts
    isSingle dstPortNum dstPorts =
        if srcExpanded
            then expandedOutputPosition srcNode srcPortNum
            else if isSingle then moveToOutputRadius portRadius t  srcNode
                             else moveToOutputRadius portRadius t' srcNode
        where
            t  = toOutputAngle trueSrc trueDst
            t1 = portAngleStart True srcPortNum srcPorts portRadius
            t2 = portAngleStop  True srcPortNum srcPorts portRadius
            t' = limitAngle t1 t2 t
            trueDst = if _dstExpanded
                then expandedInputPosition  dstNode dstPortNum
                else dstNode
            trueSrc = if srcExpanded
                then expandedOutputPosition srcNode srcPortNum
                else srcNode

connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSelf
    -> IsAlias -> Int -> Int -> Position
connectionDst srcNode dstNode srcExpanded dstExpanded dstPortNum dstPorts
    isSelf' isAlias srcPortNum srcPorts =
        if dstExpanded || isSelf'
            then trueDst
            else if isAlias then moveToInputRadius portAliasRadius t  dstNode
                            else moveToInputRadius portRadius      t' trueDst
        where
            t  = toInputAngle trueSrc trueDst
            t1 = portAngleStart True dstPortNum dstPorts portRadius
            t2 = portAngleStop  True dstPortNum dstPorts portRadius
            t'      = limitAngle t1 t2 t
            trueDst = if dstExpanded
                then expandedInputPosition dstNode dstPortNum
                else dstNode
            trueSrc = if srcExpanded
                then expandedOutputPosition srcNode srcPortNum
                else srcNode

-- Graph Calculations

expandedPortPosition :: Bool -> Position -> Int -> Position
expandedPortPosition input nodePosition portNumber =
    move
    (Vector2
        ((nodeExpandedWidth/2) * (if input then (-1) else 1))
        (lineHeight * (fromIntegral portNumber))
    )
    nodePosition

expandedInputPosition, expandedOutputPosition :: Position -> Int -> Position
expandedInputPosition  = expandedPortPosition True
expandedOutputPosition = expandedPortPosition False

-- Math Calculations

limitAngle :: Angle -> Angle -> Angle -> Angle
limitAngle opening closing current =
    if current < opening then opening
                         else if current > closing then closing
                                                   else current

toAngle :: Position -> Position -> Angle
toAngle srcPosition dstPosition =
    if srcPosition == dstPosition
        then 0
        else if srcX < dstX
            then t
            else t + pi
    where
        t    = atan $ (srcY - dstY) / (srcX - dstX)
        srcX = srcPosition ^. x
        srcY = srcPosition ^. y
        dstX = dstPosition ^. x
        dstY = dstPosition ^. y

toInputAngle :: Position -> Position -> Angle
toInputAngle srcPosition dstPosition =
    if srcX <= dstX
        then 0.5*pi - t
        else if srcY < dstY
            then -0.5*pi - t
            else  1.5*pi - t
    where
        t    = atan $ (dstY - srcY) / (dstX - srcX)
        srcX = srcPosition ^. x
        srcY = srcPosition ^. y
        dstX = dstPosition ^. x
        dstY = dstPosition ^. y

toOutputAngle :: Position -> Position -> Angle
toOutputAngle srcPosition dstPosition =
    if srcX <= dstX
        then 0.5*pi + t
        else if srcY < dstY
            then t + 1.5*pi
            else t - 0.5*pi
    where
        t    = atan $ (dstY - srcY) / (dstX - srcX)
        srcX = srcPosition ^. x
        srcY = srcPosition ^. y
        dstX = dstPosition ^. x
        dstY = dstPosition ^. y

moveToOutputRadius :: Radius -> Angle -> Position -> Position
moveToOutputRadius r t = move
    $ Vector2 (r * (cos $ t - pi/2)) (r * (sin $ t - pi/2))

moveToInputRadius :: Radius -> Angle -> Position -> Position
moveToInputRadius r t = move
    $ Vector2 (-r * (cos $ t - pi/2)) (r * (sin $ t - pi/2))


