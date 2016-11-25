{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Node (
    module React.Store.Node,
    module X,
) where

import           Control.DeepSeq             (NFData)
import           React.Flux
import           Utils.PreludePlus

import           Object.Widget.Node as X


type Store = Node

data Action = OnClick
            deriving (Show, Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform a s = print a >> return s

type Ref = ReactStore Node

create :: MonadIO m => Node -> m Ref
create = liftIO . mkStore

dispatch :: Ref -> Action -> [SomeStoreAction]
dispatch s a = [SomeStoreAction s a]
