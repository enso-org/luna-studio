{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Empire.Env where

import Prologue

import qualified Empire.Empire as Empire

import Bus.Data.Message              (Message)
import Control.Concurrent.MVar       (MVar)
import Control.Concurrent.STM.TChan  (TChan)
import Empire.Data.Graph             (CommandState (..))
import LunaStudio.API.AsyncUpdate    (AsyncUpdate)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Maybe (CommandState Empire.Env)
               , _empireNotif :: Empire.CommunicationEnv
               , _toBusChan   :: TChan Message
               }
makeLenses ''Env

make :: TChan Message
     -> TChan AsyncUpdate
     -> MVar Empire.TCRequest
     -> IO Env
make toBus fromEmpire tc =
    pure $ Env Nothing (Empire.CommunicationEnv fromEmpire tc) toBus

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
