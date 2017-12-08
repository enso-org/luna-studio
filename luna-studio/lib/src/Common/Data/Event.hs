module Common.Data.Event where

import Common.Prelude


consName :: Show a => a -> String
consName = head . words . show

class Show a => EventName a where
    eventName :: a -> String
    eventName = consName
