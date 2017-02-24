{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Control.Exception      (handle)
import           Data.ScreenPosition    (ScreenPosition (ScreenPosition), Vector2 (Vector2))
import           Data.Size              (Size (Size))
import           GHCJS.Foreign.Callback
import           GHCJS.Types            (JSException (JSException))

import qualified JS.Config              as Config
import           Luna.Studio.Prelude



data InputSidebar = InputSidebar { _inputSidebarPosition :: ScreenPosition
                                 , _inputSidebarSize     :: Size
                                 } deriving (Default, Eq, Generic, Show)

makeLenses ''InputSidebar

data OutputSidebar = OutputSidebar { _outputSidebarPosition :: ScreenPosition
                                   , _outputSidebarSize     :: Size
                                   } deriving (Default, Eq, Generic, Show)

makeLenses ''OutputSidebar

data Scene = Scene
        { _position      :: ScreenPosition
        , _size          :: Size
        , _inputSidebar  :: Maybe InputSidebar
        , _outputSidebar :: Maybe OutputSidebar
        } deriving (Default, Eq, Generic, Show)

makeLenses ''Scene

sceneId :: JSString
sceneId = Config.prefix "Graph"

appId :: JSString
appId = Config.prefix "app"

inputSidebarId, outputSidebarId :: JSString
inputSidebarId  = Config.prefix "edge-sidebar--i"
outputSidebarId = Config.prefix "edge-sidebar--o"


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
    scenePos   <- ScreenPosition .: Vector2 <$> sceneLeft <*> sceneTop
    sceneSiz   <- Size .: Vector2 <$> sceneWidth <*> sceneHeight
    inputSPos  <- handle (\JSException {} -> return Nothing) $ Just <$> (ScreenPosition .: Vector2 <$> inputSidebarLeft <*> inputSidebarTop)
    inputSSiz  <- handle (\JSException {} -> return Nothing) $ Just <$> (Size .: Vector2 <$> inputSidebarWidth <*> inputSidebarHeight)
    outputSPos <- handle (\JSException {} -> return Nothing) $ Just <$> (ScreenPosition .: Vector2 <$> outputSidebarLeft <*> outputSidebarTop)
    outputSSiz <- handle (\JSException {} -> return Nothing) $ Just <$> (Size .: Vector2 <$> outputSidebarWidth <*> outputSidebarHeight)
    let inputSidebar' = if isJust inputSPos && isJust inputSSiz then
                Just $ InputSidebar (fromJust inputSPos) (fromJust inputSSiz)
            else Nothing
        outputSidebar' = if isJust outputSPos && isJust outputSSiz then
                Just $ OutputSidebar (fromJust outputSPos) (fromJust outputSSiz)
            else Nothing
    return $ Just $ Scene scenePos sceneSiz inputSidebar' outputSidebar'
