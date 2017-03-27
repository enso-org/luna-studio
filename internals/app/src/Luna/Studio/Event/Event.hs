{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Event where

import           Data.Aeson                    (ToJSON)

import qualified Luna.Studio.Event.Batch       as Batch
import qualified Luna.Studio.Event.Connection  as Connection
import qualified Luna.Studio.Event.CustomEvent as CustomEvent
import qualified Luna.Studio.Event.Debug       as Debug
import           Luna.Studio.Event.Internal    (InternalEvent)
-- import           Luna.Studio.Event.Shortcut    (ShortcutEvent)
import           Luna.Studio.Event.Text        (TextEvent)
-- import           Luna.Studio.Event.UI          (UIEvent)
import           Luna.Studio.Prelude



data Event = Init
           | Atom                        InternalEvent
           | Batch                         Batch.Event
           | Connection               Connection.Event
           | CustomEvent             CustomEvent.Event
           | Debug                         Debug.Event
           | Tick
          --  | Shortcut                    ShortcutEvent
           | Text                            TextEvent
        --    | UI                                UIEvent
           deriving (Generic, Show, NFData)

makeLenses ''Event

instance ToJSON Event

name :: Getter Event String
name = to $ head . words . show
