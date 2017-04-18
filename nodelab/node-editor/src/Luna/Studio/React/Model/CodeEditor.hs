module Luna.Studio.React.Model.CodeEditor where

import           Luna.Prelude



data CodeEditor = CodeEditor
        { _visible :: Bool
        , _code   :: Text
        } deriving (Show, Eq)

makeLenses ''CodeEditor

instance Default CodeEditor where
    def = CodeEditor True def
