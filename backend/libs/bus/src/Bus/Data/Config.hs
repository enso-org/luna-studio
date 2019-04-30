module Bus.Data.Config where

import Prologue

data Config = Config
    { _pubSocketAddress :: String
    , _subSocketAddress :: String
    } deriving (Show)

makeLenses ''Config
