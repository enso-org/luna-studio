module JS.ConnectionPen
    ( beginPath
    , drawSegment
    , requestWidgetsBetween
    , registerCallback
    , clearCanvas
    , endPath
    , toJSArray
    ) where

import           GHCJS.Foreign.Callback
import           JavaScript.Array
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector


foreign import javascript safe "connectionPen.beginPath($1, $2, $3)" beginPath'   :: Int -> Int -> Bool -> IO ()

beginPath :: Vector2 Int -> Bool -> IO ()
beginPath (Vector2 x y) tpe = beginPath' x y tpe

foreign import javascript safe "connectionPen.endPath()"             endPath      :: IO ()
foreign import javascript safe "connectionPen.clearCanvas()"         clearCanvas  :: IO ()
foreign import javascript safe "connectionPen.drawSegment($1, $2)"   drawSegment' :: Int -> Int -> IO ()

drawSegment :: Vector2 Int -> IO ()
drawSegment (Vector2 x y) = drawSegment' x y

foreign import javascript safe "connectionPen.requestWidgetsBetween($1, $2, $3, $4)"
    requestWidgetsBetween' :: Int -> Int -> Int -> Int -> IO ()

requestWidgetsBetween :: Vector2 Int -> Vector2 Int -> IO ()
requestWidgetsBetween (Vector2 ax ay) (Vector2 bx by) = requestWidgetsBetween' ax ay bx by

foreign import javascript safe "connectionPen.callback = $1"
    registerCallback' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "connectionPen.callback = function(){ return null; }"
    unregisterCallback' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe "$r = $1" toJSArray :: JSVal -> JSArray

registerCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCallback' wrappedCallback
    return $ unregisterCallback' wrappedCallback >> releaseCallback wrappedCallback
