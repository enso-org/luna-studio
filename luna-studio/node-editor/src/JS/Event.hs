{-# LANGUAGE DeriveAnyClass #-}
module JS.Event where

import           Common.Prelude
import           Data.Aeson.Types        (ToJSON)
import           LunaStudio.Data.NodeLoc (NodeLoc)


data PortInfo = PortInfo { portId :: Text
                         } deriving (Eq, Generic, Show)

instance ToJSON PortInfo

data NodeInfo = NodeInfo { nodeLoc  :: NodeLoc
                         , nodeName :: Text
                         , portInfo :: Maybe PortInfo
                         } deriving (Eq, Generic, Show)

instance ToJSON NodeInfo

data SearcherInfo = SearcherInfo { input :: Text
                                 } deriving (Eq, Generic, Show)

instance ToJSON SearcherInfo

data GraphInfo = GraphInfo { nodeInfo     :: Maybe NodeInfo
                           , searcherInfo :: Maybe SearcherInfo
                           } deriving (Default, Eq, Generic, Show)

instance ToJSON GraphInfo


data NodeEditorEvent = GraphEvent GraphInfo deriving (Default, Eq, Generic, Show)

instance ToJSON NodeEditorEvent


data Event = Event { name      :: Text
                   , eventInfo :: NodeEditorEvent
                   } deriving (Eq, Generic, Show)

instance ToJSON Event
