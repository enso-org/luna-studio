{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Edge where

import           Data.Aeson           (FromJSON, ToJSON)
import           Empire.API.Data.Node (NodeId)
import           Luna.Studio.Prelude
import           React.Flux           (MouseEvent)



data Event = AddPort       NodeId
           | MouseMove     MouseEvent NodeId
           | RemovePort
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
