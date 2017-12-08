{-# LANGUAGE OverloadedStrings #-}

module Common.Analytics  where

import           Common.Prelude
import           Data.Aeson        (Value, toJSON)
import           GHCJS.Marshal     (toJSVal_aeson)
import qualified JS.Analytics      as JS
import           Common.Data.Event (EventName(..))


class (EventName a, Show a) => IsTrackedEvent a where
    isTracked :: a -> Bool
    isTracked = const True
    eventData :: a -> Value
    eventData = const $ toJSON (Nothing :: Maybe ())

track :: (MonadIO m, IsTrackedEvent event) => event-> m ()
track event = when (isTracked event) $
    liftIO $ JS.track (convert $ eventName event) =<< toJSVal_aeson (eventData event)


data Event = AppStarted deriving (Show)

instance EventName Event
instance IsTrackedEvent Event
