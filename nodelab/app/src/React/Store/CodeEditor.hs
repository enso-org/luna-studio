{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.CodeEditor where

import           Control.DeepSeq   (NFData)
import           Utils.PreludePlus



data CodeEditor = CodeEditor { _code   :: Text }

makeLenses ''CodeEditor

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance Default CodeEditor where
    def = CodeEditor def
