{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Data.Container.Class                       (usedIxes)
import Data.Graph                                 (Node, Link, Ref, Edge, Cluster, nodeStore)
import Old.Luna.Syntax.Model.Network.Builder.Term (NetGraph, NetNode, NetLayers, NetCluster)
import Old.Luna.Runtime.Dynamics                  (Static)
import Old.Luna.Syntax.Model.Network.Term         (Draft)

type ASTNode       = NetNode
type ASTEdge       = Link ASTNode
type AST           = NetGraph

type NodeRef       = Ref Node ASTNode
type EdgeRef       = Ref Edge ASTEdge
type ClusRef       = Ref Cluster NetCluster

type UncoveredNode = Draft Static NetLayers

astNull :: AST -> Bool
astNull ast = ast ^. _Wrapped . nodeStore . to usedIxes == [0]
