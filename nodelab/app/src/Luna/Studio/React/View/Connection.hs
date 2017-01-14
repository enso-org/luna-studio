{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Empire.API.Data.PortRef            (InPortRef)
import qualified Event.UI                           as UI
import           Luna.Studio.Action.Geometry        (connectionWidth)
import           Luna.Studio.Data.Color             (toJSString)
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

--TODO: move & refactor: the list is inversed
mergeList :: [a] -> [a] -> [a]
mergeList [] [] = []
mergeList [] ys = ys
mergeList xs [] = xs
mergeList (x:xs) ys = mergeList xs (x:ys)

lineReact :: Position -> Position -> [PropertyOrHandler ViewEventHandler] -> ReactElementM ViewEventHandler ()
lineReact src dst b = do
    let a = [ "x1" $= (fromString $ show2 $ src ^. x)
            , "y1" $= (fromString $ show2 $ src ^. y)
            , "x2" $= (fromString $ show2 $ dst ^. x)
            , "y2" $= (fromString $ show2 $ dst ^. y)
            ]
    line_ (mergeList a b) mempty

connection :: ReactView (Ref App, Connection)
connection = React.defineView name $ \(ref, model) -> do
        let connId      = model ^. Connection.connectionId
            src         = model ^. Connection.from
            dst         = model ^. Connection.to
            mid         = averagePosition src dst
            color       = "stroke"      $= (toJSString $ model ^. Connection.color  )
            width       = "strokeWidth" $= (fromString $ show2   connectionWidth    )
            widthSelect = "strokeWidth" $= (fromString $ show2 $ connectionWidth * 4)
            eventSrc    = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Source)
            eventDst    = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.ModifyConnection m connId Destination)
        g_ [ "className" $= "connection" ] $ do
            g_ [ "className" $= "connection__src" ] $ do
                lineReact src mid [ width, color ]
                lineReact src mid [ widthSelect, eventSrc ]
            g_ [ "className" $= "connection__dst" ] $ do
                lineReact mid dst [ width, color ]
                lineReact mid dst [ widthSelect, eventDst ]

connection_ :: Ref App -> InPortRef -> Connection -> ReactElementM ViewEventHandler ()
connection_ ref inPortRef model = React.viewWithSKey connection (fromString $ show inPortRef) (ref, model) mempty

currentConnection :: ReactView CurrentConnection
currentConnection = React.defineView name $ \model -> do
        let src   = model ^. Connection.currentFrom
            dst   = model ^. Connection.currentTo
            color = "stroke"      $= (toJSString $ model ^. Connection.currentColor)
            width = "strokeWidth" $= (fromString $ show2 connectionWidth           )
        lineReact src dst [ width, color ]

currentConnection_ :: CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ model = React.viewWithSKey currentConnection "current-connection" model mempty
