{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Control.Exception      (handle)
import           Data.ScreenPosition    (ScreenPosition, fromDoubles)
import           Data.Size              (Size)
import qualified Data.Size              as Size
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
sceneId = undefined--Config.prefix "Graph"

appId :: JSString
appId = undefined--Config.prefix "app"

inputSidebarId, outputSidebarId :: JSString
inputSidebarId  = undefined--Config.prefix "edge-sidebar--i"
outputSidebarId = undefined--Config.prefix "edge-sidebar--o"


-- foreign import javascript safe "document.getElementById($1).offsetWidth"  elementWidth  :: JSString -> IO Double
-- foreign import javascript safe "document.getElementById($1).offsetHeight" elementHeight :: JSString -> IO Double
-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left" elementLeft :: JSString -> IO Double
-- foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"  elementTop  :: JSString -> IO Double
-- foreign import javascript safe "new ResizeObserver($2).observe(document.getElementById($1))" onResize' :: JSString -> Callback (IO ()) -> IO ()

onSceneResize :: IO () -> IO (IO ())
onSceneResize handler = undefined --do
    -- callback <- asyncCallback handler
    -- onResize' appId callback
    -- return $ releaseCallback callback

sceneWidth, sceneHeight, sceneLeft, sceneTop :: MonadIO m => m Double
sceneWidth  = undefined --liftIO $ elementWidth  sceneId
sceneHeight = undefined --liftIO $ elementHeight sceneId
sceneLeft   = undefined --liftIO $ elementLeft   sceneId
sceneTop    = undefined --liftIO $ elementTop    sceneId

inputSidebarWidth, inputSidebarHeight, inputSidebarLeft, inputSidebarTop, outputSidebarWidth, outputSidebarHeight, outputSidebarLeft, outputSidebarTop :: MonadIO m => m Double
inputSidebarWidth   = undefined  --liftIO $ elementWidth  inputSidebarId
inputSidebarHeight  = undefined  --liftIO $ elementHeight inputSidebarId
inputSidebarLeft    = undefined  --liftIO $ elementLeft   inputSidebarId
inputSidebarTop     = undefined  --liftIO $ elementTop    inputSidebarId
outputSidebarWidth  = undefined  --liftIO $ elementWidth  outputSidebarId
outputSidebarHeight = undefined  --liftIO $ elementHeight outputSidebarId
outputSidebarLeft   = undefined  --liftIO $ elementLeft   outputSidebarId
outputSidebarTop    = undefined  --liftIO $ elementTop    outputSidebarId



get :: MonadIO m => m (Maybe Scene)
get = undefined --liftIO . handle (\JSException {} -> return Nothing) $ do
    -- scenePos   <-      fromDoubles <$> sceneLeft <*> sceneTop
    -- sceneSiz   <- Size.fromDoubles <$> sceneWidth <*> sceneHeight
    -- inputSPos  <- handle (\JSException {} -> return Nothing) $ Just <$> (     fromDoubles <$> inputSidebarLeft   <*> inputSidebarTop)
    -- inputSSiz  <- handle (\JSException {} -> return Nothing) $ Just <$> (Size.fromDoubles <$> inputSidebarWidth  <*> inputSidebarHeight)
    -- outputSPos <- handle (\JSException {} -> return Nothing) $ Just <$> (     fromDoubles <$> outputSidebarLeft  <*> outputSidebarTop)
    -- outputSSiz <- handle (\JSException {} -> return Nothing) $ Just <$> (Size.fromDoubles <$> outputSidebarWidth <*> outputSidebarHeight)
    -- return $ Just $ Scene scenePos
    --                       sceneSiz
    --                       (InputSidebar  <$> inputSPos  <*> inputSSiz)
    --                       (OutputSidebar <$> outputSPos <*> outputSSiz)
