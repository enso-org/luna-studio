module Luna.Studio.Action.Graph.CodeUpdate where

import           Data.Text                     (Text)
import           Luna.Studio.Action.CodeEditor (setCode)
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.State.Global      (State)

updateCode :: Text -> Command State ()
updateCode = setCode
