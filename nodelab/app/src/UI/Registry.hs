module UI.Registry where

import           Luna.Studio.Prelude

import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))
import           Object.Widget      (WidgetId, fromWidgetId)
import           UI.Widget          (UIWidget)


foreign import javascript safe "common.registry[$1]"            lookup' :: Int -> IO JSVal
foreign import javascript safe "common.registry[$1] = $2"     register' :: Int -> JSVal -> IO ()
foreign import javascript safe "delete common.registry[$1]" unregister' :: Int -> IO ()

lookup :: UIWidget b => WidgetId -> IO b
lookup oid = pFromJSVal <$> lookup' (fromWidgetId oid)

register :: UIWidget b => WidgetId -> b -> IO ()
register oid widget = register' (fromWidgetId oid) (pToJSVal widget)

unregister :: WidgetId -> IO ()
unregister = unregister' . fromWidgetId
