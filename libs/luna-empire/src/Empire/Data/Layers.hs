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
  , SpanLength
  , SpanOffset
  , attachEmpireLayers
  ) where

import Empire.Prelude

import           Control.Lens.Iso         (from)
import           LunaStudio.Data.NodeId   (NodeId)
import           LunaStudio.Data.NodeMeta (NodeMeta)
import           LunaStudio.Data.PortRef  (OutPortRef)

import           Control.Monad.Raise            (Throws)
import           Data.TypeDesc
import           Luna.IR                        hiding (Import, Marker, String)
import qualified Luna.IR.Layer.Type             as IR (Type)
import           Luna.IR.ToRefactor2
import           Luna.Syntax.Text.Parser.Marker (MarkedExprMap)
import           OCI.IR.Class                   (Import)
import           OCI.Pass
import           OCI.Pass.Definition            (makePass)
import           Type.Any
import           Data.Text.Position             (Delta(..))
import qualified Prologue                       as Prologue (mempty)


type TypeLayer = IR.Type

data Marker
type instance LayerData Marker t = Maybe OutPortRef

initNodeMarker :: Req m '[Editor // Layer // AnyExpr // Marker] => Listener (New // Expr l) m
initNodeMarker = listener $ \(t, _) -> putLayer @Marker t Nothing
makePass 'initNodeMarker

importNodeMarker :: Req m '[Writer // Layer // AnyExpr // Marker] => Listener (Import // Expr l) m
importNodeMarker = listener $ \(t, _, ls) -> when (getTypeDesc_ @Marker ^. from typeDesc `elem` ls) $ putLayer @Marker t Nothing
makePass 'importNodeMarker

data Meta
type instance LayerData Meta t = Maybe NodeMeta

data SpanOffset
type instance LayerData SpanOffset t = Delta

data SpanLength
type instance LayerData SpanLength t = Delta

initMeta :: Req m '[Editor // Layer // AnyExpr // Meta] => Listener (New // Expr l) m
initMeta = listener $ \(t, _) -> putLayer @Meta t Nothing
makePass 'initMeta

importMeta :: Req m '[Writer // Layer // AnyExpr // Meta] => Listener (Import // Expr l) m
importMeta = listener $ \(t, _, ls) -> when (getTypeDesc_ @Meta ^. from typeDesc `elem` ls) $ putLayer @Meta t Nothing
makePass 'importMeta

initSpanLength :: Req m '[Editor // Layer // AnyExpr // SpanLength] => Listener (New // Expr l) m
initSpanLength = listener $ \(t, _) -> putLayer @SpanLength t mempty
makePass 'initSpanLength

importSpanLength :: Req m '[Writer // Layer // AnyExpr // SpanLength] => Listener (Import // Expr l) m
importSpanLength = listener $ \(t, _, ls) -> when (getTypeDesc_ @SpanLength ^. from typeDesc `elem` ls) $ putLayer @SpanLength t mempty
makePass 'importSpanLength

initSpanOffset :: Req m '[Editor // Layer // AnyExprLink // SpanOffset] => Listener (New // ExprLink l1 l2) m
initSpanOffset = listener $ \(t, _) -> putLayer @SpanOffset t mempty
makePass 'initSpanOffset

importSpanOffset :: Req m '[Writer // Layer // AnyExprLink // SpanOffset] => Listener (Import // ExprLink l1 l2) m
importSpanOffset = listener $ \(t, _, ls) -> when (getTypeDesc_ @SpanOffset ^. from typeDesc `elem` ls) $ putLayer @SpanOffset t mempty
makePass 'importSpanOffset

attachEmpireLayers :: (MonadPassManager m, Throws IRError m) => m ()
attachEmpireLayers = do
    addExprEventListener @Meta        initMetaPass
    addExprEventListener @Meta        importMetaPass
    addExprEventListener @Marker      initNodeMarkerPass
    addExprEventListener @Marker      importNodeMarkerPass
    addExprEventListener @SpanLength  initSpanLengthPass
    addExprEventListener @SpanLength  importSpanLengthPass
    addExprLinkEventListener @SpanOffset  initSpanOffsetPass
    addExprLinkEventListener @SpanOffset  importSpanOffsetPass
    attachLayer 10 (getTypeDesc @Meta)        (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @Marker)      (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @SpanLength)  (getTypeDesc @AnyExpr)
    attachLayer 10 (getTypeDesc @SpanOffset)  (getTypeDesc @AnyExprLink)
