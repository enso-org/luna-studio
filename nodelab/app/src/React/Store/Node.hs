{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Node (
    module React.Store.Node,
    module X,
) where

import           Object.Widget.Node as X
import           React.Flux
import           Utils.PreludePlus




instance StoreData Node where
    transform _ = return

type Ref = ReactStore Node

create :: MonadIO m => Node -> m Ref
create = liftIO . mkStore
