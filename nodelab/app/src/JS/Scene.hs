{-# LANGUAGE DeriveAnyClass #-}
module JS.Scene where

import           Data.Position       (Position (Position), Vector2 (Vector2))
import           Data.Size           (Size (Size))
import           Luna.Studio.Prelude


data Scene = Scene
        { _position :: Position
        , _size     :: Size
        } deriving (Default, Generic, Show)

makeLenses ''Scene

foreign import javascript safe "document.getElementById('Graph').offsetWidth"  sceneWidth  :: IO Double
foreign import javascript safe "document.getElementById('Graph').offsetHeight" sceneHeight :: IO Double
foreign import javascript safe "document.getElementById('Graph').getBoundingClientRect().left" sceneLeft :: IO Double
foreign import javascript safe "document.getElementById('Graph').getBoundingClientRect().top"  sceneTop  :: IO Double


get :: MonadIO m => m Scene
get = liftIO $ Scene <$> (Position .: Vector2 <$> sceneLeft <*> sceneTop)
                     <*> (Size .: Vector2 <$> sceneWidth <*> sceneHeight)
