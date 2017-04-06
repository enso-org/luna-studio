{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.Model.Node.ExpressionNodeProperties where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import           Luna.Studio.React.Model.Port                (PortsMap)



data NodeProperties = NodeProperties
                    { _nodeLoc               :: NodeLoc
                    , _ports                 :: PortsMap
                    , _name                  :: Text
                    , _isLiteral             :: Bool
                    , _execTime              :: Maybe Integer
                    , _visualizationsEnabled :: Bool
                    } deriving (Eq)

makeLenses ''NodeProperties

fromNode :: ExpressionNode -> NodeProperties
fromNode node = NodeProperties
    {- nodeLoc               -} (node ^. Node.nodeLoc)
    {- ports                 -} (node ^. Node.ports)
    {- name                  -} (node ^. Node.name)
    {- isLiteral             -} (Node.isLiteral node)
    {- execTime              -} (node ^. Node.execTime)
    {- visualizationsEnabled -} (node ^. Node.visualizationsEnabled)
