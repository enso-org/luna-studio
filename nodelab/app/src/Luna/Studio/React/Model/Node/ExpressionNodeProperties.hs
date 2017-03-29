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
                    , _nameEdit              :: Maybe Text
                    , _execTime              :: Maybe Integer
                    , _visualizationsEnabled :: Bool
                    } deriving (Eq)

makeLenses ''NodeProperties

fromNode :: ExpressionNode -> NodeProperties
fromNode node = NodeProperties (node ^. Node.nodeLoc)
                               (node ^. Node.ports)
                               (node ^. Node.name)
                               (Node.isLiteral node)
                               (node ^. Node.nameEdit)
                               (node ^. Node.execTime)
                               (node ^. Node.visualizationsEnabled)
