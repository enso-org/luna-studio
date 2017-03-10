{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.Data.Layers (
    Marker
  , Meta
  , InputsLayer
  , TypeLayer
  , TCData
  , TCError(ImportError, UnificationError)
  , tcErrors
  , attachEmpireLayers
  ) where

import Empire.Prelude

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeMeta (NodeMeta)
import           Empire.API.Data.PortRef  (OutPortRef)

import           Control.Monad.Raise (Throws)
import           Data.TypeDesc
import           Luna.IR                  hiding (String)
import qualified Luna.IR.Layer.Type       as IR (Type)
import           Luna.IR.ToRefactor2
import           OCI.Pass
import           OCI.Pass.Definition      (makePass)
import           Type.Any


type TypeLayer = IR.Type

data Marker
type instance LayerData Marker t = Maybe OutPortRef

initNodeMarker :: Req m '[Editor // Layer // AnyExpr // Marker] => Listener New (Expr l) m
initNodeMarker = listener $ \(t, _) -> writeLayer @Marker Nothing t
makePass 'initNodeMarker

data Meta
type instance LayerData Meta t = Maybe NodeMeta

initMeta :: Req m '[Editor // Layer // AnyExpr // Meta] => Listener New (Expr l) m
initMeta = listener $ \(t, _) -> writeLayer @Meta Nothing t
makePass 'initMeta

data InputsLayer
type instance LayerData InputsLayer t = [SomeExprLink]

initInputsLayer :: Req m '[Editor // Layer // AnyExpr // InputsLayer] => Listener New (Expr l) m
initInputsLayer = listener $ \(t, _) -> writeLayer @InputsLayer [] t
makePass 'initInputsLayer

data TCError a = ImportError (Maybe a) String
               | UnificationError a

data TCDataMock = TCDataMock { _tcErrors :: [TCError SomeExpr] }

makeLenses ''TCDataMock

data TCData
type instance LayerData TCData t = TCDataMock

initTcData :: Req m '[Editor // Layer // AnyExpr // TCData] => Listener New (Expr l) m
initTcData = listener $ \(t, _) -> writeLayer @TCData (TCDataMock []) t
makePass 'initTcData

attachEmpireLayers :: (MonadPassManager m, Throws IRError m) => m ()
attachEmpireLayers = do
    addExprEventListener @Meta initMetaPass
    addExprEventListener @Marker initNodeMarkerPass
    addExprEventListener @InputsLayer initInputsLayerPass
    addExprEventListener @TCData initTcDataPass
    attachLayer 10 (getTypeDesc @Meta)  (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @Marker) (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @InputsLayer) (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @TCData) (getTypeDesc @AnyExpr)
