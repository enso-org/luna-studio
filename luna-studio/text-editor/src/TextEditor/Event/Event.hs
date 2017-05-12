{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Event where

import           Data.Aeson                  (ToJSON)

import           Common.Prelude
import qualified TextEditor.Event.Batch      as Batch
import qualified TextEditor.Event.Connection as Connection
import           TextEditor.Event.Internal   (InternalEvent)
import           TextEditor.Event.Text       (TextEvent)



data Event = Init
           | Atom                        InternalEvent
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | Text                            TextEvent
           deriving (Generic, Show, NFData)


instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
