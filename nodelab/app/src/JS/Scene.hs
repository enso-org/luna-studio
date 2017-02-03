{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Scene where

import           Data.Position       (Position (Position), Vector2 (Vector2))
import           Data.Size           (Size (Size))
import qualified JS.Config           as Config
import           Luna.Studio.Prelude


data Scene = Scene
        { _position :: Position
        , _size     :: Size
        } deriving (Default, Generic, Show)

makeLenses ''Scene

sceneId :: JSString
sceneId = Config.prefix "Graph"


foreign import javascript safe "document.getElementById($1).offsetWidth"  elementWidth  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).offsetHeight" elementHeight :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left" elementLeft :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"  elementTop  :: JSString -> IO Double

sceneWidth, sceneHeight, sceneLeft, sceneTop :: MonadIO m => m Double
sceneWidth  = liftIO $ elementWidth  sceneId
sceneHeight = liftIO $ elementHeight sceneId
sceneLeft   = liftIO $ elementLeft   sceneId
sceneTop    = liftIO $ elementTop    sceneId

get :: MonadIO m => m Scene
get = Scene <$> (Position .: Vector2 <$> sceneLeft <*> sceneTop)
            <*> (Size .: Vector2 <$> sceneWidth <*> sceneHeight)
