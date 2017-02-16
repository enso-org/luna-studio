{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Edge where

import           Data.Aeson           (FromJSON, ToJSON)
import           Empire.API.Data.Node (NodeId)
import           Luna.Studio.Prelude



data Event = AddPort NodeId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
