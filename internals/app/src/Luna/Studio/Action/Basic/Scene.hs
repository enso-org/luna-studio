module Luna.Studio.Action.Basic.Scene where

import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForEdgeNodes)
import           Luna.Studio.Action.Command              (Command)
import qualified Luna.Studio.Action.State.Scene          as Scene
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                (State)

updateScene :: Command State ()
updateScene = Scene.updateScene >> void redrawConnectionsForEdgeNodes
