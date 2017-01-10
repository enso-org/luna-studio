module Luna.Studio.Action.CodeEditor
    ( setCode
    , toggle
    , codeChanged
    ) where

import           Empire.API.Data.Node               (NodeId)
import qualified JS.GoogleAnalytics                 as GA
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.CodeEditor as CodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global


setCode :: Text -> Command State ()
setCode code =
    Global.withCodeEditor $ Store.modify_ $ CodeEditor.code .~ code

toggle :: Command Global.State ()
toggle = do
    GA.sendEvent $ GA.ToggleText
    Global.withCodeEditor $ Store.modify_ $ CodeEditor.visible %~ not
    -- size <- use $ Global.camera . Camera.camera . Camera.windowSize --TODO[react] remove
    -- Camera.updateWindowSize size

codeChanged :: NodeId -> Text -> Command State ()
codeChanged nodeId newCode = do
    BatchCmd.setCode nodeId newCode
