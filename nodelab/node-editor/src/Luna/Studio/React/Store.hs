{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Luna.Studio.React.Store
    ( module Luna.Studio.React.Store
    , module X
) where

import           Control.Monad.Trans.Reader
import           React.Flux                  hiding (Event)

import           Luna.Studio.Event.Event     (Event)
import qualified Luna.Studio.Event.Event     as Event
import           Luna.Studio.Event.UI        (UIEvent)
import           Luna.Prelude         as P hiding (transform)
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Store.Ref as X



instance Typeable a => StoreData (Store a) where
    type StoreAction (Store a) = Event
    transform event store = do
        store ^. sendEvent $ event
        return store

dispatch :: Typeable a => Ref a -> UIEvent -> [SomeStoreAction]
dispatch s = dispatch' s . Event.UI

dispatch' :: Typeable a => Ref a -> Event -> [SomeStoreAction]
dispatch' s a = [SomeStoreAction s a]

create' :: (StoreData (Store a), MonadIO m) => SendEvent -> a -> m (Ref a)
create' se a = liftIO $ mkStore $ Store a se

create :: (StoreData (Store a), MonadIO m) => a -> SendEventM m (Ref a)
create a = do
    se <- ask
    create' se a

createApp :: MonadIO m => SendEvent -> m (Ref App)
createApp = runReaderT $ create def
