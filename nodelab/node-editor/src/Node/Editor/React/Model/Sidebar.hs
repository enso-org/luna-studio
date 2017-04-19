{-# LANGUAGE DeriveAnyClass #-}
module Node.Editor.React.Model.Sidebar where

import           Data.Position                     (Position, fromDoubles)
import           Data.ScreenPosition               (ScreenPosition)
import           Data.Size                         (Size, x)
import           Luna.Prelude
import           Node.Editor.React.Model.Constants (gridSize)
import           Node.Editor.React.Model.Port      (InPortId, OutPortId, getPortNumber, isSelf)


data InputSidebar = InputSidebar { _inputSidebarPosition :: ScreenPosition
                                 , _inputSidebarSize     :: Size
                                 } deriving (Default, Eq, Generic, Show)

data OutputSidebar = OutputSidebar { _outputSidebarPosition :: ScreenPosition
                                   , _outputSidebarSize     :: Size
                                   } deriving (Default, Eq, Generic, Show)

makeLenses ''InputSidebar
makeLenses ''OutputSidebar


portPositionInInputSidebar :: Size -> OutPortId -> Position
portPositionInInputSidebar sidebarSize pid = fromDoubles posX posY where
    portNum = getPortNumber pid
    posX    = sidebarSize ^. x
    posY    = (fromIntegral portNum) * gridSize

portPositionInOutputSidebar :: InPortId -> Position
portPositionInOutputSidebar pid = fromDoubles 0 posY where
    portNum = getPortNumber pid
    posY    = (fromIntegral $ if isSelf pid then 0 else portNum) * gridSize
