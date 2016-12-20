{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Empire.Data.AST where

import           Empire.Prelude

import Luna.IR (AnyExpr, AnyExprLink, EXPRESSION, IR, IRMonad, Layer, LayerData, IRBuilder,
                type Abstract, ElemScope, Attr, WorkingElem, registerLayer, readAttr, writeLayer)
import Luna.IR.Layer.Type as IR (Type)
import qualified Luna.Pass.Manager as Pass (PassManager, State)
import qualified Luna.Pass as Pass
import Luna.Pass (type Events, type Inputs, type Outputs, type Preserves)
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData, InterpreterLayer)
import System.Log (Logger, DropLogger)
import Empire.API.Data.Node (NodeId)
import Empire.API.Data.NodeMeta (NodeMeta)

data ASTState = ASTState IR (Pass.State (IRBuilder (Logger DropLogger IO)))

instance Show ASTState where
    show _ = "AST"

type AST           = ASTState
type NodeRef       = AnyExpr
type EdgeRef       = AnyExprLink

data Marker
newtype NodeMarker = NodeMarker NodeId deriving (Show, Eq)
type instance LayerData Marker t = Maybe NodeMarker
type instance Abstract  Marker = Marker
type instance Inputs    (ElemScope Marker t) = '[Layer (Abstract t) Marker, Attr WorkingElem]
type instance Outputs   (ElemScope Marker t) = '[]
type instance Events    (ElemScope Marker t) = '[]
type instance Preserves (ElemScope Marker t) = '[]

marker :: (MonadIO m, IRMonad m) => Pass.Pass (ElemScope Marker (EXPRESSION t)) m
marker = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @Marker Nothing el

marker_reg :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
marker_reg = registerLayer (typeRep @Proxy @Marker Proxy) $ \_ -> Pass.compile marker

data Meta
type instance LayerData Meta t = Maybe NodeMeta
type instance Abstract  Meta = Meta
type instance Inputs    (ElemScope Meta t) = '[Layer (Abstract t) Meta, Attr WorkingElem]
type instance Outputs   (ElemScope Meta t) = '[Layer (Abstract t) Meta]
type instance Events    (ElemScope Meta t) = '[]
type instance Preserves (ElemScope Meta t) = '[]

meta :: (MonadIO m, IRMonad m) => Pass.Pass (ElemScope Meta (EXPRESSION t)) m
meta = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @Meta Nothing el

meta_reg :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
meta_reg = registerLayer (typeRep @Proxy @Meta Proxy) $ \_ -> Pass.compile meta

data InputsLayer
type instance LayerData InputsLayer t = [EdgeRef]
type instance Abstract  InputsLayer = InputsLayer
type instance Inputs    (ElemScope InputsLayer t) = '[Layer (Abstract t) InputsLayer, Attr WorkingElem]
type instance Outputs   (ElemScope InputsLayer t) = '[Layer (Abstract t) InputsLayer]
type instance Events    (ElemScope InputsLayer t) = '[]
type instance Preserves (ElemScope InputsLayer t) = '[]

inputsLayer :: (MonadIO m, IRMonad m) => Pass.Pass (ElemScope InputsLayer (EXPRESSION t)) m
inputsLayer = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @InputsLayer [] el

inputsLayer_reg :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
inputsLayer_reg = registerLayer (typeRep @Proxy @InputsLayer Proxy) $ \_ -> Pass.compile inputsLayer

data TCData
type instance LayerData TCData t = TCDataMock
type instance Abstract  TCData = TCData
type instance Inputs    (ElemScope TCData t) = '[Layer (Abstract t) TCData, Attr WorkingElem]
type instance Outputs   (ElemScope TCData t) = '[Layer (Abstract t) TCData]
type instance Events    (ElemScope TCData t) = '[]
type instance Preserves (ElemScope TCData t) = '[]

tcData :: (MonadIO m, IRMonad m) => Pass.Pass (ElemScope TCData (EXPRESSION t)) m
tcData = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @TCData (TCDataMock []) el

tcData_reg :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
tcData_reg = registerLayer (typeRep @Proxy @TCData Proxy) $ \_ -> Pass.compile tcData

type instance LayerData InterpreterData t = InterpreterLayer
type instance Abstract InterpreterData = InterpreterData
type instance Inputs    (ElemScope InterpreterData t) = '[Layer (Abstract t) InterpreterData, Attr WorkingElem]
type instance Outputs   (ElemScope InterpreterData t) = '[Layer (Abstract t) InterpreterData]
type instance Events    (ElemScope InterpreterData t) = '[]
type instance Preserves (ElemScope InterpreterData t) = '[]

intData :: (MonadIO m, IRMonad m) => Pass.Pass (ElemScope InterpreterData (EXPRESSION t)) m
intData = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @InterpreterData def el

intData_reg :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
intData_reg = registerLayer (typeRep @Proxy @InterpreterData Proxy) $ \_ -> Pass.compile intData

registerEmpireLayers :: (MonadIO m, IRMonad m) => Pass.PassManager m ()
registerEmpireLayers = sequence_ [ marker_reg
                                 , meta_reg
                                 , inputsLayer_reg
                                 , tcData_reg
                                 , intData_reg
                                 ]

type TypeLayer = IR.Type

data TCError a = ImportError (Maybe a) String
               | UnificationError a

data TCDataMock = TCDataMock { _tcErrors :: [TCError NodeRef] }

makeLenses ''TCDataMock

astNull :: AST -> Bool
astNull = $notImplemented
