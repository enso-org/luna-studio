module Reactive.Commands.CodeEditor
    ( setCode
    , toggle
    ) where

import           Utils.PreludePlus

import qualified JS.GoogleAnalytics        as GA
import qualified React.Store               as Store
import qualified React.Store.CodeEditor    as CodeEditor
import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global



setCode :: Text -> Command State ()
setCode code =
    Global.withCodeEditor $ Store.modify_ $ CodeEditor.code .~ code

toggle :: Command Global.State ()
toggle = do
    GA.sendEvent $ GA.ToggleText
    Global.withCodeEditor $ Store.modify_ $ CodeEditor.visible %~ not
    -- size <- use $ Global.camera . Camera.camera . Camera.windowSize --TODO[react] remove
    -- Camera.updateWindowSize size
