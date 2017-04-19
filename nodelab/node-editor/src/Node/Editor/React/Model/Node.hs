{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Node.Editor.React.Model.Node
    ( module X
    , module Node.Editor.React.Model.Node
    ) where

import           Data.HashMap.Strict                         (HashMap)
import           Empire.API.Data.Node                        as X (NodeId)
import           Empire.API.Data.NodeLoc                     as X (NodeLoc (NodeLoc), NodePath (NodePath))
import           Luna.Prelude
import           Node.Editor.React.Model.IsNode              as X
import           Node.Editor.React.Model.Node.ExpressionNode as X (ExpressionNode, ExpressionNodesMap)
import           Node.Editor.React.Model.Node.NodeMeta       ()
import           Node.Editor.React.Model.Node.SidebarNode    as X (InputNode (InputNode), InputNodesMap, OutputNode (OutputNode),
                                                                   OutputNodesMap)


data Node = Expression ExpressionNode
          | Input      InputNode
          | Output     OutputNode
          deriving (Eq, Generic, NFData, Show)

makeLenses ''Node
makePrisms ''Node

type NodesMap = HashMap NodeId Node

instance HasNodeLoc Node where
    nodeLoc f (Expression node) = Expression <$> nodeLoc f node
    nodeLoc f (Input      node) = Input      <$> nodeLoc f node
    nodeLoc f (Output     node) = Output     <$> nodeLoc f node

instance HasPorts Node where
    inPortsList        (Expression node) = inPortsList  node
    inPortsList        (Input      node) = inPortsList  node
    inPortsList        (Output     node) = inPortsList  node
    outPortsList       (Expression node) = outPortsList node
    outPortsList       (Input      node) = outPortsList node
    outPortsList       (Output     node) = outPortsList node
    inPortAt  portId f (Expression node) = Expression <$> inPortAt  portId f node
    inPortAt  portId f (Input      node) = Input      <$> inPortAt  portId f node
    inPortAt  portId f (Output     node) = Output     <$> inPortAt  portId f node
    outPortAt portId f (Expression node) = Expression <$> outPortAt portId f node
    outPortAt portId f (Input      node) = Input      <$> outPortAt portId f node
    outPortAt portId f (Output     node) = Output     <$> outPortAt portId f node
