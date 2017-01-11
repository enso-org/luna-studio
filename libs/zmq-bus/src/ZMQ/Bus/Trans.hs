{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module ZMQ.Bus.Trans where

import           Control.Monad.IO.Class
import           Control.Monad.Catch           (MonadThrow)

import           Prologue
import           ZMQ.Bus.Bus            (Bus)


--FIXME[PM] : rename to BusWrapper
-- and implement as: newtype BusT a = BusT { runBusT :: Bus a} deriving (Monad, MonadIO)
-- and in other file
newtype BusT a = BusT { runBusT :: Bus a}

instance Functor BusT where
    fmap f (BusT a) = BusT $ f <$> a

instance Applicative BusT where
    pure a = BusT $ pure a
    (BusT f) <*> (BusT a) = BusT $ f <*> a

instance Monad BusT where
    (BusT a) >>= f = BusT $ a >>= runBusT . f


instance MonadIO BusT where
    liftIO a = BusT $ liftIO a

instance MonadThrow BusT where
    throwM e = BusT $ lift $ lift $ throwM e 
