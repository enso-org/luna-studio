{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Empire.Env where

import           Prologue


data MonitorEnv = MonitorEnv { _script           :: FilePath
                             , _lastActivityTime :: Integer
                             , _timeout          :: Integer
                             }

makeLenses ''MonitorEnv

instance Default MonitorEnv where
    def = MonitorEnv "" 0 0
