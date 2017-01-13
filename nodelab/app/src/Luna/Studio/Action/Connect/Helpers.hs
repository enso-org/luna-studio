module Luna.Studio.Action.Connect.Helpers
    ( createCurrentConnection
    , toValidConnection
    ) where

import           Empire.API.Data.Connection          (Connection)
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.PortRef             (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef             as PortRef
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Graph.Disconnect (localRemoveConnections)
import           Luna.Studio.Data.Color              (Color)
import           Luna.Studio.Data.Vector             (Position)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection  (CurrentConnection (CurrentConnection))
import qualified Luna.Studio.React.Model.NodeEditor  as NodeEditor
import           Luna.Studio.State.Global            (State)
import qualified Luna.Studio.State.Global            as Global


createCurrentConnection :: AnyPortRef -> Maybe Connection -> Position -> Position -> Color -> Command State ()
createCurrentConnection portRef modifiedConn srcPos dstPos color = do
    withJust modifiedConn $ \conn -> localRemoveConnections [conn ^. Connection.dst]
    let connection = CurrentConnection portRef modifiedConn srcPos dstPos color
    Global.withNodeEditor $ do
        NodeEditor.currentConnection ?= connection

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection src' dst' = (normalize' src' dst') >>= toOtherNode where
    normalize' (OutPortRef' a) (InPortRef' b) = Just (a, b)
    normalize' (InPortRef' a) (OutPortRef' b) = Just (b, a)
    normalize' _ _ = Nothing
    toOtherNode (a, b)
        | a ^. PortRef.srcNodeId /= b ^. PortRef.dstNodeId = Just (a, b)
        | otherwise                                        = Nothing
