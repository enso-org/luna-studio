module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           Empire.API.Data.GraphLocation          (GraphLocation (GraphLocation))
import           NodeEditor.Action.Basic.ProjectManager (navigateToGraph)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.State.Global                (State, workspace)


openFile :: FilePath -> Command State ()
openFile path = navigateToGraph $ GraphLocation path def

closeFile :: Command State ()
closeFile = do
    workspace .= def
    resetGraph
