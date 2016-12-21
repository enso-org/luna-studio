{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Empire.Data.Layers (
    Marker
  , Meta
  , NodeMarker(..)
  , InputsLayer
  , TypeLayer
  , TCData
  , TCError(ImportError, UnificationError)
  , tcErrors
  , attachEmpireLayers
  , registerEmpireLayers
  ) where

import Empire.Prelude

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeMeta (NodeMeta)

import           Luna.IR                  (AnyExpr, AnyExprLink, ElemScope, EXPR, EXPRESSION,
                                           IRMonad, Layer, LayerData, type Abstract,
                                           type Attr, WorkingElem, attachLayer,
                                           readAttr, registerLayer, writeLayer)
import qualified Luna.IR.Layer.Type       as IR (Type)
import           Luna.Pass                (Pass, type Events, type Inputs,
                                           type Outputs, type Preserves)
import qualified Luna.Pass                as Pass
import           Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData, InterpreterLayer)
import           Luna.Pass.Manager        (PassManager)


type TypeLayer = IR.Type

data Marker
newtype NodeMarker = NodeMarker NodeId deriving (Show, Eq)

type instance LayerData Marker t = Maybe NodeMarker
type instance Abstract  Marker = Marker
type instance Inputs    (ElemScope Marker t) = '[Layer (Abstract t) Marker, Attr WorkingElem]
type instance Outputs   (ElemScope Marker t) = '[]
type instance Events    (ElemScope Marker t) = '[]
type instance Preserves (ElemScope Marker t) = '[]

marker :: (MonadIO m, IRMonad m) => Pass (ElemScope Marker (EXPRESSION t)) m
marker = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @Marker Nothing el

registerMarker :: (MonadIO m, IRMonad m) => PassManager m ()
registerMarker = registerLayer (typeRep @Proxy @Marker Proxy) $ \_ -> Pass.compile marker

data Meta
type instance LayerData Meta t = Maybe NodeMeta
type instance Abstract  Meta = Meta
type instance Inputs    (ElemScope Meta t) = '[Layer (Abstract t) Meta, Attr WorkingElem]
type instance Outputs   (ElemScope Meta t) = '[Layer (Abstract t) Meta]
type instance Events    (ElemScope Meta t) = '[]
type instance Preserves (ElemScope Meta t) = '[]

meta :: (MonadIO m, IRMonad m) => Pass (ElemScope Meta (EXPRESSION t)) m
meta = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @Meta Nothing el

registerMeta :: (MonadIO m, IRMonad m) => PassManager m ()
registerMeta = registerLayer (typeRep @Proxy @Meta Proxy) $ \_ -> Pass.compile meta

data InputsLayer
type instance LayerData InputsLayer t = [AnyExprLink]
type instance Abstract  InputsLayer = InputsLayer
type instance Inputs    (ElemScope InputsLayer t) = '[Layer (Abstract t) InputsLayer, Attr WorkingElem]
type instance Outputs   (ElemScope InputsLayer t) = '[Layer (Abstract t) InputsLayer]
type instance Events    (ElemScope InputsLayer t) = '[]
type instance Preserves (ElemScope InputsLayer t) = '[]

inputsLayer :: (MonadIO m, IRMonad m) => Pass (ElemScope InputsLayer (EXPRESSION t)) m
inputsLayer = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @InputsLayer [] el

registerInputs :: (MonadIO m, IRMonad m) => PassManager m ()
registerInputs = registerLayer (typeRep @Proxy @InputsLayer Proxy) $ \_ -> Pass.compile inputsLayer


data TCError a = ImportError (Maybe a) String
               | UnificationError a

data TCDataMock = TCDataMock { _tcErrors :: [TCError AnyExpr] }

makeLenses ''TCDataMock

data TCData
type instance LayerData TCData t = TCDataMock
type instance Abstract  TCData = TCData
type instance Inputs    (ElemScope TCData t) = '[Layer (Abstract t) TCData, Attr WorkingElem]
type instance Outputs   (ElemScope TCData t) = '[Layer (Abstract t) TCData]
type instance Events    (ElemScope TCData t) = '[]
type instance Preserves (ElemScope TCData t) = '[]

tcData :: (MonadIO m, IRMonad m) => Pass (ElemScope TCData (EXPRESSION t)) m
tcData = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @TCData (TCDataMock []) el

registerTCData :: (MonadIO m, IRMonad m) => PassManager m ()
registerTCData = registerLayer (typeRep @Proxy @TCData Proxy) $ \_ -> Pass.compile tcData

type instance LayerData InterpreterData t = InterpreterLayer
type instance Abstract InterpreterData = InterpreterData
type instance Inputs    (ElemScope InterpreterData t) = '[Layer (Abstract t) InterpreterData, Attr WorkingElem]
type instance Outputs   (ElemScope InterpreterData t) = '[Layer (Abstract t) InterpreterData]
type instance Events    (ElemScope InterpreterData t) = '[]
type instance Preserves (ElemScope InterpreterData t) = '[]

interpreterData :: (MonadIO m, IRMonad m) => Pass (ElemScope InterpreterData (EXPRESSION t)) m
interpreterData = do
    (el, _) <- readAttr @WorkingElem
    writeLayer @InterpreterData def el

registerInterpreterData :: (MonadIO m, IRMonad m) => PassManager m ()
registerInterpreterData = registerLayer (typeRep @Proxy @InterpreterData Proxy) $
    \_ -> Pass.compile interpreterData

registerEmpireLayers :: (MonadIO m, IRMonad m) => PassManager m ()
registerEmpireLayers = do
    registerMarker
    registerMeta
    registerInputs
    registerTCData
    registerInterpreterData

attachEmpireLayers :: (MonadIO m, IRMonad m) => PassManager m ()
attachEmpireLayers = do
    attachLayer 10 (typeRep' @Meta)  (typeRep' @EXPR)
    attachLayer 10 (typeRep' @Marker) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @InputsLayer) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @InterpreterData) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @TCData) (typeRep' @EXPR)
