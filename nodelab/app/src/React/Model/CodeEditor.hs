module React.Model.CodeEditor where

import           Utils.PreludePlus



data CodeEditor = CodeEditor
        { _visible :: Bool
        , _code   :: Text
        }

makeLenses ''CodeEditor

instance Default CodeEditor where
    def = CodeEditor True def
