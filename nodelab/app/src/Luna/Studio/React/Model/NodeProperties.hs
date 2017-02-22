{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.Model.NodeProperties where

import           Data.Map.Lazy                (Map)
import           Empire.API.Data.Port         (PortId)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (Node, NodeId)
import qualified Luna.Studio.React.Model.Node as Node
import           Luna.Studio.React.Model.Port (Port)



data NodeProperties = NodeProperties
                { _nodeId                :: NodeId
                , _ports                 :: Map PortId Port
                , _name                  :: Text
                , _isLiteral             :: Bool
                , _nameEdit              :: Maybe Text
                , _execTime              :: Maybe Integer
                , _visualizationsEnabled :: Bool
                } deriving (Eq)

makeLenses ''NodeProperties

fromNode :: Node -> NodeProperties
fromNode node = NodeProperties (node ^. Node.nodeId)
                           (node ^. Node.ports)
                           (node ^. Node.name)
                           (node ^. Node.isLiteral)
                           (node ^. Node.nameEdit)
                           (node ^. Node.execTime)
                           (node ^. Node.visualizationsEnabled)
