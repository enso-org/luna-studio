module React.Store.CodeEditor where

import           Utils.PreludePlus



data CodeEditor = CodeEditor { _code   :: Text }

makeLenses ''CodeEditor

instance Default CodeEditor where
    def = CodeEditor def
