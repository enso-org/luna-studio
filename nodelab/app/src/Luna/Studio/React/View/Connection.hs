{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Data.Position                      (Position, averagePosition, x, y)
import           Empire.API.Data.PortRef            (InPortRef)
import           Luna.Studio.Action.Geometry        (connectionWidth)
import           Luna.Studio.Data.Color             (toJSString)
import qualified Luna.Studio.Event.UI               as UI
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection (ModifiedEnd (Destination, Source))
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.App        (App)
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Numeric                            (showFFloat)
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "connection"

show2 :: Double -> JSString
show2 a = convert $ showFFloat (Just 2) a "" -- limit Double to two decimal numbers

--TODO: move & refactor: the list is inversed
mergeList :: [a] -> [a] -> [a]
mergeList [] [] = []
mergeList [] ys = ys
mergeList xs [] = xs
mergeList (x1:xs) ys = mergeList xs (x1:ys)

line :: Position -> Position -> [PropertyOrHandler ViewEventHandler] -> ReactElementM ViewEventHandler ()
line src dst b = do
    let a = [ "x1" $= show2 (src ^. x)
            , "y1" $= show2 (src ^. y)
            , "x2" $= show2 (dst ^. x)
            , "y2" $= show2 (dst ^. y)
            ]
    line_ (mergeList a b) mempty

connection :: ReactView (Ref App, Connection)
connection = React.defineView name $ \(ref, model) -> do
    let connId      = model ^. Connection.connectionId
        src         = model ^. Connection.from
        dst         = model ^. Connection.to
        mid         = averagePosition src dst
        color       = "stroke"      $= toJSString (model ^. Connection.color)
        width       = "strokeWidth" $= show2   connectionWidth
        widthSelect = "strokeWidth" $= show2 (connectionWidth * 4)
        eventSrc    = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Source)
        eventDst    = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Destination)
    g_
        [ "className" $= "luna-connection"
        , "key"       $= "connection"] $ do
        line src dst [ width, color ]
        g_
            [ "className" $= "luna-connection__src"
            , "key"       $= "src" ] $ do
            line src mid [ width, "key" $= "1" ]
            line src mid [ widthSelect, eventSrc, "key" $= "2" ]
        g_
            [ "className" $= "luna-connection__dst"
            , "key" $= "dst" ] $ do
            line mid dst [ width, "key" $= "1" ]
            line mid dst [ widthSelect, eventDst, "key" $= "2" ]

connection_ :: Ref App -> InPortRef -> Connection -> ReactElementM ViewEventHandler ()
connection_ ref inPortRef model = React.viewWithSKey connection (jsShow inPortRef) (ref, model) mempty

currentConnection :: ReactView CurrentConnection
currentConnection = React.defineView name $ \model -> do
    let src   = model ^. Connection.currentFrom
        dst   = model ^. Connection.currentTo
        color = "stroke"      $= toJSString (model ^. Connection.currentColor)
        width = "strokeWidth" $= show2 connectionWidth
    line src dst [ width, color ]

currentConnection_ :: CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ model = React.viewWithSKey currentConnection "current-connection" model mempty
