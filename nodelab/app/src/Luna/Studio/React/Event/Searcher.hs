{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Searcher where

import           Data.Aeson          (FromJSON, ToJSON)
import           React.Flux          (KeyboardEvent)

import           Luna.Studio.Prelude



data Event = InputChanged Text
           | Accept
           | AcceptInput
           | AcceptEntry Int
           | MoveDown
           | MoveUp
           | MoveRight
           | MoveLeft
           | KeyDown KeyboardEvent
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
