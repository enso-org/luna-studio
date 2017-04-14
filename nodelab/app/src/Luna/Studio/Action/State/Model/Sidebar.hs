module Luna.Studio.Action.State.Model.Sidebar where

import           Data.Position                       (Position)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.NodeEditor (getLayout)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Layout      (inputSidebarPortPosition, outputSidebarPortPosition)
import           Luna.Studio.React.Model.Port        (InPort, OutPort)
import           Luna.Studio.State.Global            (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForSidebarNodes may be needed after use of those function

getInputSidebarPortPosition :: OutPort -> Command State (Maybe Position)
getInputSidebarPortPosition p = inputSidebarPortPosition p <$> getLayout

getOutputSidebarPortPosition :: InPort -> Command State (Maybe Position)
getOutputSidebarPortPosition p = outputSidebarPortPosition p <$> getLayout
