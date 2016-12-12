{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Empire.Data.AST where

import           Prologue

import Luna.IR (IRMonad, AnyExpr, AnyExprLink, Accessibles, ExprNet, ExprLinkNet,
                ExprLayers, Model, ExprLinkLayers, LayerData)
import Luna.IR.Layer.Type as IR (Type)
import Luna.Pass.Evaluation.Interpreter.Value (Value)

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

data TCData = TCData
type instance LayerData TCData t = TCDataMock

type TypeLayer = IR.Type

data TCError a = ImportError (Maybe a) String
               | UnificationError a

data TCDataMock = TCDataMock { _tcErrors :: [TCError NodeRef] }

makeLenses ''TCDataMock

astNull :: AST -> Bool
astNull = $notImplemented
-- astNull ast = ast ^. elems . to Map.null
