{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.NodeEditor where

import           Control.DeepSeq             (NFData)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Utils.PreludePlus

import           Empire.API.Data.Node        (NodeId)
import           React.Store.Node            (Node)
import           React.Store.Ref             (Ref)



data NodeEditor = NodeEditor { _nodes   :: HashMap NodeId (Ref Node)
                             }

makeLenses ''NodeEditor

data Action = Action
            deriving (Show, Generic, NFData, Typeable)

instance Default NodeEditor where
    def = NodeEditor HashMap.empty
