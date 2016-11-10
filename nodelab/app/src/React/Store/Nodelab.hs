{-# LANGUAGE TypeFamilies #-}

module React.Store.Nodelab where

import           Utils.PreludePlus
import React.Flux

data Props = Props Int
           deriving (Show)

data Store = Store Int
           deriving (Show)


data Action = Add
            | Sub
            deriving (Show)

instance StoreData Store where
    type StoreAction Store = Action
    transform action (Store i) = do
        case action of
            Add -> return $ Store $ i + 1
            Sub -> return $ Store $ i - 1
