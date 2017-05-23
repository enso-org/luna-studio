{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Empire.Env where

import           Prologue

import           Control.Concurrent.STM.TChan  (TChan)
import           Control.Concurrent.MVar       (MVar)
import           Empire.Data.Graph             (Graph)
import qualified Empire.Empire                 as Empire
import           LunaStudio.API.AsyncUpdate    (AsyncUpdate)
import           LunaStudio.Data.GraphLocation (GraphLocation (..))
import           ZMQ.Bus.Data.Message          (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Empire.Env
               , _empireNotif :: Empire.CommunicationEnv
               , _formatted   :: Bool
               , _toBusChan   :: TChan Message
               , _projectRoot :: FilePath
               } deriving (Show)
makeLenses ''Env

make :: TChan Message -> TChan AsyncUpdate -> TChan (GraphLocation, Graph, Bool) -> MVar Empire.SymbolMap -> FilePath -> Env
make toBus fromEmpire tc sm = Env def (Empire.CommunicationEnv fromEmpire tc sm) True toBus

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
