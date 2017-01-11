{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Data.ConnectionPen where

import           Control.DeepSeq         (NFData)
import           Empire.API.Data.Node    (NodeId)
import           Luna.Studio.Data.Vector (Position)
import           Luna.Studio.Prelude


data Mode = Connecting | Disconnecting deriving (Eq, Generic, Show)

data ConnectionPen = ConnectionPen { _mode            :: Mode
                                   , _history         :: [Position]
                                   , _lastVisitedNode :: Maybe NodeId
                                   } deriving (Eq, Generic, Show)

makeLenses ''ConnectionPen
