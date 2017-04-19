{-# LANGUAGE OverloadedStrings #-}
module Node.Editor.React.View.Connection where

import           Data.Position                      (Position, averagePosition, x, y)
import qualified Node.Editor.Event.UI               as UI
import           Luna.Prelude
import           Node.Editor.React.Event.Connection (ModifiedEnd (Destination, Source))
import qualified Node.Editor.React.Event.Connection as Connection
import           Node.Editor.React.Model.App        (App)
import           Node.Editor.React.Model.Connection (PosConnection, PosHalfConnection)
import qualified Node.Editor.React.Model.Connection as Connection
import           Node.Editor.React.Store            (Ref, dispatch)
import qualified Node.Editor.React.View.Style       as Style
import           Numeric                            (showFFloat)
import           React.Flux                         as React


name :: JSString
name = "connection"

show2 :: Double -> JSString
show2 a = convert $ showFFloat (Just 2) a "" -- limit Double to two decimal numbers

show0 :: Double -> JSString
show0 a = convert $ showFFloat (Just 0) a "" -- limit Double to two decimal numbers


--TODO: move & refactor: the list is inversed
mergeList :: [a] -> [a] -> [a]
mergeList [] [] = []
mergeList [] ys = ys
mergeList xs [] = xs
mergeList (x1:xs) ys = mergeList xs (x1:ys)

line :: Position -> Position -> [PropertyOrHandler ViewEventHandler] -> ReactElementM ViewEventHandler ()
line src dst b = do
    let a = [ "x1" $= show0 (src ^. x)
            , "y1" $= show0 (src ^. y)
            , "x2" $= show0 (dst ^. x)
            , "y2" $= show0 (dst ^. y)
            ]
    line_ (mergeList a b) mempty

connection :: ReactView (Ref App, PosConnection)
connection = React.defineView name $ \(ref, model) -> do
    let connId   = model ^. Connection.connectionId
        src      = model ^. Connection.srcPos
        dst      = model ^. Connection.dstPos
        mid      = averagePosition src dst
        eventSrc = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.MouseDown m connId Source)
        eventDst = onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.MouseDown m connId Destination)
    g_
        [ "key"       $= "connection"
        , "className" $= Style.prefix "connection"
        ] $ do
        line src dst
            [ "key"       $= "line"
            , "className" $= Style.prefix "connection__line"
            , "stroke"    $= convert (model ^. Connection.color)
            ]
        g_
            [ "className" $= Style.prefix "connection__src"
            , "key"       $= "src"
            ] $ do
            line src mid
                [ "key"       $= "1"
                , "className" $= Style.prefix "connection__line"
                ]
            line src mid
                [ "key"       $= "2"
                , "className" $= Style.prefix "connection__select"
                , eventSrc
                ]
        g_
            [ "className" $= Style.prefix "connection__dst"
            , "key" $= "dst" ] $ do
            line mid dst
                [ "key"       $= "1"
                , "className" $= Style.prefix "connection__line"
                ]
            line mid dst
                [ "key"       $= "2"
                , "className" $= Style.prefix "connection__select"
                , eventDst
                ]

connection_ :: Ref App -> PosConnection -> ReactElementM ViewEventHandler ()
connection_ ref model = React.viewWithSKey connection (jsShow $ model ^. Connection.connectionId) (ref, model) mempty

halfConnection :: ReactView PosHalfConnection
halfConnection = React.defineView name $ \model -> do
    let src   = model ^. Connection.srcPos
        dst   = model ^. Connection.dstPos
        color = "stroke" $= convert (model ^. Connection.color)
    line src dst [ color, "className" $= Style.prefix "connection__line" ]

halfConnection_ :: Int -> PosHalfConnection -> ReactElementM ViewEventHandler ()
halfConnection_ key model = React.viewWithSKey halfConnection (fromString $ "half-connection" <> show key) model mempty
