{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.Scene where

import           Data.ScreenPosition             (ScreenPosition)
import           Data.Size                       (Size)
import           Luna.Prelude
import           Luna.Studio.React.Model.Sidebar (InputSidebar, OutputSidebar)


data Scene = Scene
        { _position      :: ScreenPosition
        , _size          :: Size
        , _inputSidebar  :: Maybe InputSidebar
        , _outputSidebar :: Maybe OutputSidebar
        } deriving (Default, Eq, Generic, Show)

makeLenses ''Scene
