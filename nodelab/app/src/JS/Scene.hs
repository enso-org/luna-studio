{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Control.Exception      (handle)
import           Data.Position          (Position (Position), Vector2 (Vector2))
import           Data.Size              (Size (Size))
import           GHCJS.Foreign.Callback
import           GHCJS.Types            (JSException (JSException))

import qualified JS.Config              as Config
import           Luna.Studio.Prelude



data Scene = Scene
        { _position :: Position
        , _size     :: Size
        } deriving (Default, Eq, Generic, Show)

makeLenses ''Scene

sceneId :: JSString
sceneId = Config.prefix "Graph"

appId :: JSString
appId = Config.prefix "app"

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

get :: MonadIO m => m (Maybe Scene)
get = liftIO . handle (\JSException {} -> return Nothing) $
    Just .: Scene <$> (Position .: Vector2 <$> sceneLeft <*> sceneTop)
                  <*> (Size .: Vector2 <$> sceneWidth <*> sceneHeight)
