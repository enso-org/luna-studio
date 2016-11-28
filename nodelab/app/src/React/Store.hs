{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store
    ( module React.Store
    , module X
) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.Trans.Reader
import           React.Flux
import           Utils.PreludePlus          as P hiding (transform)

import           React.Store.App            (App (App))
import qualified React.Store.Node           as Node
import           React.Store.Ref            as X



data UIEvent = NodeEvent Node.Event
             deriving (Show, Generic, NFData, Typeable)

instance Typeable a => StoreData (Store a) where
    type StoreAction (Store a) = UIEvent
    transform event store = do
        print event
        return $ store

dispatch :: Typeable a => Ref a -> UIEvent -> [SomeStoreAction]
dispatch s a = [SomeStoreAction s a]

create' :: (StoreData (Store a), MonadIO m) => SendEvent -> a -> m (Ref a)
create' se a = liftIO $ mkStore $ Store a se

create :: (StoreData (Store a), MonadIO m) => a -> SendEventM m (Ref a)
create a = do
    se <- ask
    create' se a

createApp :: MonadIO m => SendEvent -> m (Ref App)
createApp = runReaderT (create =<< (App <$> create def <*> create def))
