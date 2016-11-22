{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.NodeEditor where

import           Control.DeepSeq      (NFData)
import           React.Flux
import           Utils.PreludePlus

import qualified Reactive.State.Graph as Graph


data Store = Store
           deriving (Show, Generic)


data Action = AddNode
            | DelNode
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform action store = do
        case action of
            AddNode -> putStrLn "call AddNode" >> return store
            DelNode -> putStrLn "call DelNode" >> return store

type Ref = ReactStore Store

create :: IO Ref
create = mkStore $ Store
