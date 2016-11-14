{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-} -- for NFData

module React.Store.Nodelab where

import           Utils.PreludePlus
import React.Flux
import Control.DeepSeq (NFData)



data Props = Props Int
           deriving (Show)

data Store = Store Int
           deriving (Show)


data Action = Add
            | Sub
            deriving (Show, Generic, Typeable, NFData)

instance StoreData Store where
    type StoreAction Store = Action
    transform action (Store i) = do
        case action of
            Add -> return $ Store $ i + 1
            Sub -> return $ Store $ i - 1

store :: ReactStore Store
store = mkStore $ Store 1
