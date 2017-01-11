{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Empire.API.Data.PortRef            (InPortRef)
import qualified Event.UI                           as UI
import           Luna.Studio.Action.Geometry        (connectionWidth)
import           Luna.Studio.Data.Color             (toJSString)
import           Luna.Studio.Data.Vector            (x, y)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Event.Connection (ModifiedEnd (Destination, Source))
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dispatch, dt)
import           Numeric                            (showFFloat)
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "connection-editor"

show2 :: Double -> String
show2 a = showFFloat (Just 4) a "" -- limit Double to two decimal numbers

connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection' = connectionStore ^. dt
            connId      = connection' ^. Connection.connectionId
            src         = connection' ^. Connection.from
            dst         = connection' ^. Connection.to
            color       = connection' ^. Connection.color
            srcX        = src ^. x
            srcY        = src ^. y
            dstX        = dst ^. x
            dstY        = dst ^. y
            midX        = (srcX + dstX) / 2
            midY        = (srcY + dstY) / 2
            width       = fromString $ show connectionWidth
            widthSelect = fromString $ show $ connectionWidth * 4
        g_ [ "className" $= "connection" ] $ do
            g_ [ "className" $= "connection__src" ] $ do
                line_
                    [ "x1"          $= (fromString $ show2 srcX)
                    , "y1"          $= (fromString $ show2 srcY)
                    , "x2"          $= (fromString $ show2 midX)
                    , "y2"          $= (fromString $ show2 midY)
                    , "stroke"      $= toJSString color
                    , "strokeWidth" $= width
                    ] mempty
                line_
                    [ onMouseDown $ \e m -> stopPropagation e : dispatch connectionRef (UI.ConnectionEvent $ Connection.ModifyConnection m connId Source)
                    , "className"   $= "connection__select"
                    , "x1"          $= (fromString $ show2 srcX)
                    , "y1"          $= (fromString $ show2 srcY)
                    , "x2"          $= (fromString $ show2 midX)
                    , "y2"          $= (fromString $ show2 midY)
                    , "strokeWidth" $= widthSelect
                    ] mempty
            g_ [ "className" $= "connection__dst" ] $ do
                line_
                    [ "x1"          $= (fromString $ show2 midX)
                    , "y1"          $= (fromString $ show2 midY)
                    , "x2"          $= (fromString $ show2 dstX)
                    , "y2"          $= (fromString $ show2 dstY)
                    , "stroke"      $= toJSString color
                    , "strokeWidth" $= width
                    ] mempty
                line_
                    [ onMouseDown $ \e m -> stopPropagation e : dispatch connectionRef (UI.ConnectionEvent $ Connection.ModifyConnection m connId Destination)
                    , "className"   $= "connection__select"
                    , "x1"          $= (fromString $ show2 midX)
                    , "y1"          $= (fromString $ show2 midY)
                    , "x2"          $= (fromString $ show2 dstX)
                    , "y2"          $= (fromString $ show2 dstY)
                    , "strokeWidth" $= widthSelect
                    ] mempty


connection_ :: InPortRef -> Ref Connection -> ReactElementM ViewEventHandler ()
connection_ inPortRef connectionRef = React.viewWithSKey (connection connectionRef) (fromString $ show inPortRef) () mempty


currentConnection :: Ref CurrentConnection -> ReactView ()
currentConnection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection'= connectionStore ^. dt
            src        = connection' ^. Connection.currentFrom
            dst        = connection' ^. Connection.currentTo
            color      = connection' ^. Connection.currentColor
            x1         = fromString $ show2 $ src ^. x
            y1         = fromString $ show2 $ src ^. y
            x2         = fromString $ show2 $ dst ^. x
            y2         = fromString $ show2 $ dst ^. y
            width      = fromString $ show connectionWidth
        line_
            [ "x1"          $= x1
            , "y1"          $= y1
            , "x2"          $= x2
            , "y2"          $= y2
            , "stroke"      $= toJSString color
            , "strokeWidth" $= width
            ] mempty


currentConnection_ :: Ref CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ connectionRef = React.viewWithSKey (currentConnection connectionRef) "current-connection" () mempty
