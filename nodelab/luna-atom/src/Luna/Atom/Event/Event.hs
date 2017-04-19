{-# LANGUAGE DeriveAnyClass #-}
module Luna.Atom.Event.Event where

import           Data.Aeson                    (ToJSON)

import qualified Luna.Atom.Event.Batch       as Batch
import qualified Luna.Atom.Event.Connection  as Connection
import qualified Luna.Atom.Event.Debug       as Debug
import           Luna.Atom.Event.Internal    (InternalEvent)
import           Luna.Atom.Event.Text        (TextEvent)
import           Luna.Prelude



data Event = Init
           | Atom                        InternalEvent
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | Debug                         Debug.Event
           | Tick
           | Text                            TextEvent
           deriving (Generic, Show, NFData)


instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
