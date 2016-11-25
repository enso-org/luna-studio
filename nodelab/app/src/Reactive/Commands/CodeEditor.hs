module Reactive.Commands.CodeEditor
    ( setCode
    ) where

import           Utils.PreludePlus

import qualified React.Store               as Store
import qualified React.Store.CodeEditor    as CodeEditor
import qualified Reactive.State.Global     as Global

import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)



setCode :: Text -> Command State ()
setCode code =
    Global.inCodeEditor $ Store.modify_ $ CodeEditor.code .~ code
