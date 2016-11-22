{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Function.Output where

import           Control.DeepSeq            (NFData)
import           Object.Widget.FunctionPort (FunctionPort)
import           Object.Widget.Node         (Node)
import           React.Flux
import           Utils.PreludePlus



data Store = Store { _input :: FunctionPort }
           deriving (Show, Generic)

data Action = ModifyPort
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform action store = do
        case action of
            ModifyPort -> putStrLn "call ModifyPort" >> return store

type Ref = ReactStore Store

create :: FunctionPort -> IO Ref
create = mkStore . Store
