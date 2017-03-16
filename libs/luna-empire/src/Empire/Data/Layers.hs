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
  , TypeLayer
  , attachEmpireLayers
  ) where

import Empire.Prelude

import           Control.Lens.Iso         (from)
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

importNodeMarker :: Req m '[Writer // Layer // AnyExpr // Marker] => Listener Import (Expr l) m
importNodeMarker = listener $ \(t, _, ls) -> when (getTypeDesc_ @Marker ^. from typeDesc `elem` ls) $ writeLayer @Marker Nothing t
makePass 'importNodeMarker

data Meta
type instance LayerData Meta t = Maybe NodeMeta

initMeta :: Req m '[Editor // Layer // AnyExpr // Meta] => Listener New (Expr l) m
initMeta = listener $ \(t, _) -> writeLayer @Meta Nothing t
makePass 'initMeta

importMeta :: Req m '[Writer // Layer // AnyExpr // Meta] => Listener Import (Expr l) m
importMeta = listener $ \(t, _, ls) -> when (getTypeDesc_ @Meta ^. from typeDesc `elem` ls) $ writeLayer @Meta Nothing t
makePass 'importMeta

attachEmpireLayers :: (MonadPassManager m, Throws IRError m) => m ()
attachEmpireLayers = do
    addExprEventListener @Meta   initMetaPass
    addExprEventListener @Meta   importMetaPass
    addExprEventListener @Marker initNodeMarkerPass
    addExprEventListener @Marker importNodeMarkerPass
    attachLayer 10 (getTypeDesc @Meta)  (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @Marker) (getTypeDesc @AnyExpr)
