{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Control.Exception              (handle)
import           Data.ScreenPosition            (fromDoubles)
import qualified LunaStudio.Data.Size           as Size
import           GHCJS.Foreign.Callback
import           GHCJS.Types                    (JSException (JSException))

import           Common.Prelude
import qualified JS.Config                      as Config
import           NodeEditor.React.Model.Layout  (Scene (Scene))
import           NodeEditor.React.Model.Sidebar (InputSidebar (InputSidebar), OutputSidebar (OutputSidebar))


sceneId :: JSString
sceneId = Config.prefix "Graph"

appId :: JSString
appId = Config.prefix "app"

inputSidebarId, outputSidebarId :: JSString
inputSidebarId  = Config.prefix "sidebar--i"
outputSidebarId = Config.prefix "sidebar--o"


foreign import javascript safe "document.getElementById($1).offsetWidth"  elementWidth  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).offsetHeight" elementHeight :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left" elementLeft :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"  elementTop  :: JSString -> IO Double
foreign import javascript safe "new ResizeObserver($2).observe(document.getElementById($1))" onResize' :: JSString -> Callback (IO ()) -> IO ()

onSceneResize :: IO () -> IO (IO ())
onSceneResize handler = do
    callback <- asyncCallback handler
    onResize' appId callback
    return $ releaseCallback callback

sceneWidth, sceneHeight, sceneLeft, sceneTop :: MonadIO m => m Double
sceneWidth  = liftIO $ elementWidth  sceneId
sceneHeight = liftIO $ elementHeight sceneId
sceneLeft   = liftIO $ elementLeft   sceneId
sceneTop    = liftIO $ elementTop    sceneId

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
