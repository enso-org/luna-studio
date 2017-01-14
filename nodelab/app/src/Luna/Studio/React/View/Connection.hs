{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Empire.API.Data.PortRef            (InPortRef)
import qualified Event.UI                           as UI
import           Luna.Studio.Action.Geometry        (connectionWidth)
import           Luna.Studio.Data.Color             (toJSString, Color)
import           Luna.Studio.Data.Vector            (x, y, Position, averagePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection (ModifiedEnd (Destination, Source))
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import           Luna.Studio.React.Model.App        (App)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Numeric                            (showFFloat)
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "connection-editor"

show2 :: Double -> String
show2 a = showFFloat (Just 2) a "" -- limit Double to two decimal numbers


--FIXME: Why it does not work with [PropertyOrHandler handler]
mergeList :: [a] -> [a] -> [a]
mergeList xs      [] = xs
mergeList []      ys = ys
mergeList []      [] = []
mergeList (x: xs) ys = mergeList xs (x:ys)

--FIXME: Position -> Position -> [PropertyOrHandler handler] -> ReactElementM ViewEventHandler ()
lineReact :: Position -> Position -> Maybe Double -> Maybe Color -> Maybe (PropertyOrHandler ViewEventHandler) -> ReactElementM ViewEventHandler ()
lineReact src dst mwidth mcolor mevent = do
    let a = [ "x1" $= (fromString $ show2 $ src ^. x)
            , "y1" $= (fromString $ show2 $ src ^. y)
            , "x2" $= (fromString $ show2 $ dst ^. x)
            , "y2" $= (fromString $ show2 $ dst ^. y)
            ]
        b = case mwidth of
            Nothing      -> a
            (Just width) -> ("strokeWidth" $= (fromString $ show width)):a
        c = case mcolor of
            Nothing      -> b
            (Just color) -> ("stroke" $= (toJSString color)):b
        d = case mevent of
            Nothing      -> c
            (Just event) -> (event):c
    line_ d mempty

connection :: ReactView (Ref App, Connection)
connection = React.defineView name $ \(ref, model) -> do
        let connId      = model ^. Connection.connectionId
            src         = model ^. Connection.from
            dst         = model ^. Connection.to
            mid         = averagePosition src dst
            color       = model ^. Connection.color
            width       = connectionWidth
            widthSelect = connectionWidth * 4
            event1      = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Source)
            event2      = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Destination)
        g_ [ "className" $= "connection" ] $ do
            g_ [ "className" $= "connection__src" ] $ do
                lineReact src mid (Just width      ) (Just color) Nothing
                lineReact src mid (Just widthSelect) Nothing      (Just event1)
            g_ [ "className" $= "connection__dst" ] $ do
                lineReact mid dst (Just width      ) (Just color) Nothing
                lineReact mid dst (Just widthSelect) Nothing      (Just event2)

connection_ :: Ref App -> InPortRef -> Connection -> ReactElementM ViewEventHandler ()
connection_ ref inPortRef model = React.viewWithSKey connection (fromString $ show inPortRef) (ref, model) mempty


currentConnection :: ReactView CurrentConnection
currentConnection = React.defineView name $ \model -> do
        let src   = model ^. Connection.currentFrom
            dst   = model ^. Connection.currentTo
            color = model ^. Connection.currentColor
            width = connectionWidth
        lineReact src dst (Just width) (Just color) Nothing


currentConnection_ :: CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ model = React.viewWithSKey currentConnection "current-connection" model mempty
