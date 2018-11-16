{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Empire.Data.Layers (
    TypeLayer
  , Marker
  , Meta
  , SpanLength
  , SpanOffset
  ) where

import Empire.Prelude ()

import qualified Data.Graph.Component.Node.Layer  as Layer
import Luna.Pass.Data.Layer.PortMarker (PortMarker)
import Luna.Pass.Data.Layer.NodeMeta   (Meta)
import Luna.Pass.Data.Layer.SpanLength (SpanLength)
import Luna.Pass.Data.Layer.SpanOffset (SpanOffset)

type TypeLayer = Layer.Type
type Marker = PortMarker
