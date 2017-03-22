{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Empire.Data.Parser where

import           Empire.Prelude       hiding (mempty)
import           Prologue             (Text, mempty)

import           Control.Monad.Catch  (MonadCatch(..))
import           Control.Monad.State  (StateT, runStateT, get, put)
import qualified Control.Monad.State.Dependent as DepState
import qualified Data.Map             as Map
import           Data.Foldable        (toList)
import           Empire.Data.Graph    (ASTState(..), Graph, withVis)
import qualified Empire.Data.Graph    as Graph (ast, breadcrumbHierarchy)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers   (Marker, Meta, TypeLayer)
import           Empire.Empire        (Command)

import           Data.Event           (Emitters, type (//))
import           Data.Graph.Class     (MonadRefLookup(..), Net)
import           Data.TypeDesc        (getTypeDesc)
import           Luna.IR              hiding (get, put, match)
import           Luna.IR.Layer.Succs  (Succs)
import           OCI.Pass.Class       (Inputs, Outputs, Preserves, KnownPass)
import qualified OCI.Pass.Class       as Pass (SubPass, eval')
import qualified OCI.Pass.Manager     as Pass (PassManager, Cache, setAttr, State)

import           System.Log                                   (Logger, DropLogger, dropLogs)
import           Luna.Pass.Data.ExprRoots                     (ExprRoots(..))
import           Luna.Pass.Resolution.Data.UnresolvedVars     (UnresolvedVars(..))
import           Luna.Pass.Resolution.Data.UnresolvedConses   (UnresolvedConses(..), NegativeConses(..))
import qualified Luna.Pass.Resolution.AliasAnalysis           as AliasAnalysis
import qualified Luna.Syntax.Text.Parser.Parser               as Parser
import qualified Luna.Syntax.Text.Parser.Parsing              as Parsing
import qualified Luna.Syntax.Text.Parser.CodeSpan             as CodeSpan
import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
import qualified Data.SpanTree                                as SpanTree
import           Luna.Syntax.Text.Source                      (Source, SourceTree)

import qualified OCI.IR.Repr.Vis                   as Vis
import qualified Control.Monad.State.Dependent.Old as DepOld

type ParserLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // Succs,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID,
                      AnyExpr // CodeSpan.CodeSpan,
                      AnyExpr // Parser.Parser]

type ParserEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink]

data ParserPass
type instance Abstract   ParserPass = ParserPass
type instance Inputs     Net   ParserPass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer ParserPass = ParserLayers
type instance Inputs     Attr  ParserPass = '[Source, Parser.ParsedExpr, SourceTree, MarkedExprMap, Parser.ReparsingStatus]
type instance Inputs     Event ParserPass = '[]

type instance Outputs    Net   ParserPass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer ParserPass = ParserLayers
type instance Outputs    Attr  ParserPass = '[Source, Parser.ParsedExpr, SourceTree, MarkedExprMap, Parser.ReparsingStatus]
type instance Outputs    Event ParserPass = ParserEmitters

type instance Preserves        ParserPass = '[]
