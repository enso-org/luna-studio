{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Luna.IR (IRMonad, AnyExpr, AnyExprLink, Accessibles, ExprNet, ExprLinkNet,
                ExprLayers, Model, ExprLinkLayers, LayerData)

import Empire.API.Data.Node (NodeId)
import Empire.API.Data.NodeMeta (NodeMeta)

type AST           = ()
type NodeRef       = AnyExpr
type EdgeRef       = AnyExprLink

data Marker = Marker
newtype NodeMarker = NodeMarker NodeId deriving (Show, Eq)
type instance LayerData Marker t = Maybe NodeMarker

data Meta = Meta
type instance LayerData Meta t = Maybe NodeMeta

data Inputs = Inputs
type instance LayerData Inputs t = [EdgeRef]

astNull :: AST -> Bool
astNull = $notImplemented
-- astNull ast = ast ^. elems . to Map.null
