{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Common.Prelude
import           Control.Exception              (handle)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure             (pFromJSVal)
import           GHCJS.Types                    (JSException (JSException))
import qualified JS.Mount                      as Mount
import           LunaStudio.Data.ScreenPosition (ScreenPosition, fromDoubles)
import qualified LunaStudio.Data.Size           as Size
import           NodeEditor.React.Model.Layout  (Scene (Scene))
import           NodeEditor.React.Model.Sidebar (InputSidebar (InputSidebar), OutputSidebar (OutputSidebar))


appId :: JSString
appId = Mount.prefix "app"

planeCanvasId :: JSString
planeCanvasId = Mount.prefix "plane-canvas"

inputSidebarId, outputSidebarId :: JSString
inputSidebarId  = Mount.prefix "sidebar--i"
outputSidebarId = Mount.prefix "sidebar--o"


foreign import javascript safe "document.getElementById($1).offsetWidth"  elementWidth  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).offsetHeight" elementHeight :: JSString -> IO Double
foreign import javascript safe "movementHandler = function(e) { ($1)(e.movementX, e.movementY);};"  onMouseMove'  :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

onMovement :: (ScreenPosition -> IO ()) -> IO (IO ())
onMovement handler = do
    callback <- asyncCallback2 (\x y -> handler $ fromDoubles (pFromJSVal x) (pFromJSVal y))
    onMouseMove' callback
    return $ releaseCallback callback


get :: MonadIO m => m (Maybe Scene)
get = pure Nothing