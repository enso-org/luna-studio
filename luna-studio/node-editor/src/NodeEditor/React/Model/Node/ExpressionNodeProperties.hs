{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.Model.Node.ExpressionNodeProperties
( module NodeEditor.React.Model.Node.ExpressionNodeProperties
, module X
) where

import           Common.Prelude
import           NodeEditor.React.Model.IsNode              as X (HasNodeLoc (..), HasPorts (..))
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Model.Port                (InPort, InPortTree, OutPort, OutPortTree)
import qualified NodeEditor.React.Model.Port                as Port


data NodeProperties = NodeProperties
                    { _nodeLoc'              :: NodeLoc
                    , _inPorts               :: InPortTree InPort
                    , _outPorts              :: OutPortTree OutPort
                    , _name                  :: Maybe Text
                    , _execTime              :: Maybe Integer
                    , _visualizationsEnabled :: Bool
                    } deriving (Eq)

makeLenses ''NodeProperties

fromNode :: ExpressionNode -> NodeProperties
fromNode node = NodeProperties
    {- nodeLoc               -} (node ^. Node.nodeLoc)
    {- inPorts               -} (node ^. Node.inPorts)
    {- outPorts              -} (node ^. Node.outPorts)
    {- name                  -} (node ^. Node.name)
    {- execTime              -} (node ^. Node.execTime)
    {- visualizationsEnabled -} (node ^. Node.visualizationsEnabled)

instance HasNodeLoc NodeProperties where
    nodeLoc = nodeLoc'

instance HasPorts NodeProperties where
    inPortsList = Port.inPortTreeLeafs . view inPorts
    outPortsList = Port.outPortTreeLeafs . view outPorts
    inPortAt  pid = inPorts . ix pid
    outPortAt pid = outPorts . ix pid
