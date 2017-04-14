module Luna.Studio.Action.Basic.Scene where

import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForSidebarNodes)
import           Luna.Studio.Action.Command              (Command)
import qualified Luna.Studio.Action.State.Scene          as Scene
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Layout          (Scene)
import           Luna.Studio.State.Global                (State)


updateScene :: Command State ()
updateScene = Scene.updateScene >> void redrawConnectionsForSidebarNodes

getScene :: Command State (Maybe Scene)
getScene = do
    mayScene <- Scene.getScene
    void redrawConnectionsForSidebarNodes
    return mayScene
