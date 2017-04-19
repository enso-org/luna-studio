{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Node.Editor.React.Store
    ( module Node.Editor.React.Store
    , module X
) where

import           Control.Monad.Trans.Reader
import           React.Flux                  hiding (Event)

import           Node.Editor.Event.Event     (Event)
import qualified Node.Editor.Event.Event     as Event
import           Node.Editor.Event.UI        (UIEvent)
import           Luna.Prelude         as P hiding (transform)
import           Node.Editor.React.Model.App (App)
import           Node.Editor.React.Store.Ref as X



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
