{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Common.Prelude
import           Control.Exception              (handle)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure             (pFromJSVal)
import           GHCJS.Types                    (JSException (JSException))
import qualified JS.Config                      as Config
import           LunaStudio.Data.ScreenPosition (ScreenPosition, fromDoubles)
import qualified LunaStudio.Data.Size           as Size
import           NodeEditor.React.Model.Layout  (Scene (Scene))
import           NodeEditor.React.Model.Sidebar (InputSidebar (InputSidebar), OutputSidebar (OutputSidebar))


appId :: JSString
appId = Config.prefix "app"

planeCanvasId :: JSString
planeCanvasId = Config.prefix "plane-canvas"

inputSidebarId, outputSidebarId :: JSString
inputSidebarId  = Config.prefix "sidebar--i"
outputSidebarId = Config.prefix "sidebar--o"


foreign import javascript safe "document.getElementById($1).offsetWidth"  elementWidth  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).offsetHeight" elementHeight :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left"   elementLeft   :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"    elementTop    :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().right"  elementRight  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().bottom" elementBottom :: JSString -> IO Double
foreign import javascript safe "movementHandler = function(e) { ($1)(e.movementX, e.movementY);};"  onMouseMove'  :: Callback (JSVal -> JSVal -> IO ()) -> IO ()
foreign import javascript safe "new ResizeObserver($2).observe(document.getElementById($1))" onResize' :: JSString -> Callback (IO ()) -> IO ()

onSceneResize :: IO () -> IO (IO ())
onSceneResize handler = do
    callback <- asyncCallback handler
    onResize' appId callback
    return $ releaseCallback callback

onMovement :: (ScreenPosition -> IO ()) -> IO (IO ())
onMovement handler = do
    callback <- asyncCallback2 (\x y -> handler $ fromDoubles (pFromJSVal x) (pFromJSVal y))
    onMouseMove' callback
    return $ releaseCallback callback

sceneWidth, sceneHeight, sceneLeft, sceneTop :: MonadIO m => m Double
sceneWidth  = liftIO $ elementWidth  appId
sceneHeight = liftIO $ elementHeight appId
sceneLeft   = liftIO $ elementLeft   appId
sceneTop    = liftIO $ elementTop    appId

inputSidebarWidth, inputSidebarHeight, inputSidebarLeft, inputSidebarTop, outputSidebarWidth, outputSidebarHeight, outputSidebarLeft, outputSidebarTop :: MonadIO m => m Double
inputSidebarWidth   = liftIO $ elementWidth  inputSidebarId
inputSidebarHeight  = liftIO $ elementHeight inputSidebarId
inputSidebarLeft    = liftIO $ elementLeft   inputSidebarId
inputSidebarTop     = liftIO $ elementTop    inputSidebarId
outputSidebarWidth  = liftIO $ elementWidth  outputSidebarId
outputSidebarHeight = liftIO $ elementHeight outputSidebarId
outputSidebarLeft   = liftIO $ elementLeft   outputSidebarId
outputSidebarTop    = liftIO $ elementTop    outputSidebarId



get :: MonadIO m => m (Maybe Scene)
get = liftIO . handle (\JSException {} -> return Nothing) $ do
    scenePos   <-      fromDoubles <$> sceneLeft <*> sceneTop
    sceneSiz   <- Size.fromDoubles <$> sceneWidth <*> sceneHeight
    inputSPos  <- handle (\JSException {} -> return Nothing) $ Just <$> (     fromDoubles <$> inputSidebarLeft   <*> inputSidebarTop)
    inputSSiz  <- handle (\JSException {} -> return Nothing) $ Just <$> (Size.fromDoubles <$> inputSidebarWidth  <*> inputSidebarHeight)
    outputSPos <- handle (\JSException {} -> return Nothing) $ Just <$> (     fromDoubles <$> outputSidebarLeft  <*> outputSidebarTop)
    outputSSiz <- handle (\JSException {} -> return Nothing) $ Just <$> (Size.fromDoubles <$> outputSidebarWidth <*> outputSidebarHeight)
    return $ Just $ Scene scenePos
                          sceneSiz
                          (InputSidebar  <$> ((flip (-) scenePos) <$> inputSPos) <*> inputSSiz)
                          (OutputSidebar <$> ((flip (-) scenePos) <$> outputSPos) <*> outputSSiz)
