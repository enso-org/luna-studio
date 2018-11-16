{-# LANGUAGE PatternSynonyms #-}

module Empire.Prelude (
  module X, nameToString, nameToText, stringToName, (<?!>),
  -- lens
  use, preuse, (?=), (.=), (%=), to, _Just, (?~), makeWrapped, makePrisms, zoom,
  -- luna compat
  getLayer, putLayer, modifyLayer_, getAttr, putAttr,
  AnyExpr, AnyExprLink, Expr, SubPass,
  type ASGFunction,
  type ClsASG,
  type IRSuccs,
  type LeftSpacedSpan, 
  type Link,
  type MarkedExprMap,
  type SomeExpr,
  deepDelete, deepDeleteWithWhitelist,
  inputs,
  narrowTerm,
  ociSetToList,
  source, target, matchExpr,
  ptrListToList, generalize,
  irDeleteLink,
  irDelete, link, modifyExprTerm,
  -- luna patterns
  pattern Acc,
  pattern App,
  pattern ASGFunction,
  pattern Blank,
  pattern ClsASG,
  pattern Cons,
  pattern Documented,
  pattern Grouped,
  pattern Import,
  pattern ImportHub,
  pattern ImportSrc,
  pattern Invalid,
  pattern IRString,
  pattern IRNumber,
  pattern Lam,
  pattern LeftSection, 
  pattern LeftSpacedSpan,
  pattern List,
  pattern Marked,
  pattern Marker,
  pattern MarkedExprMap,
  pattern Metadata,
  pattern Missing,
  pattern ResolvedCons,
  pattern ResolvedDef,
  pattern RightSection,
  pattern Seq,
  pattern Tuple,
  pattern Unify,
  pattern Unit,
  pattern Var,

  -- luna reexport
  Substitute.replaceSource, Destruct.deleteSubtree, Substitute.substitute,
  Layout.unsafeRelayout,
  Substitute.replace,
  
  -- layers in core
  NodeMetaLike, OutPortRefLike,
  toNodeMeta, fromNodeMeta, toPortMarker, fromPortMarker
  ) where

import qualified Data.Graph.Data.Component.Set as PtrSet
import qualified Data.Graph.Data.Component.Vector as PtrList
import qualified Data.Text.Span as Span
import qualified Data.Set as Set
import qualified Data.Graph.Component.Node.Class    as Node
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.Graph.Data.Layer.Layout as Layout
import qualified Data.Graph.Component.Edge.Destruction as DestructEdge
import qualified Data.Graph.Component.Node.Destruction as Destruct
import qualified Data.Mutable.Class as Mutable
import qualified Luna.IR as IR
import OCI.IR.Link.Class (type (*-*), Links)
import OCI.IR.Term.Class (Term, Terms)
import qualified Luna.IR.Term.Core as Ast
import qualified Luna.IR.Term.Literal as Ast
import qualified Luna.IR.Term.Ast.Class as Ast
import Data.Graph.Data.Component.Class (Component)
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Transform.Substitute as Substitute
import qualified Data.Graph.Data.Component.List as List (ComponentList(..))
import qualified Data.Graph.Component.Edge as Edge
import qualified Data.Graph.Component.Edge.Construction as Construction
import Luna.Syntax.Text.Parser.State.Marker (TermMap(..))
import Luna.Pass (Pass)
import qualified Luna.Pass.Attr as Attr
import qualified Data.Mutable.Class as Foreign
import           LunaStudio.Data.Port    (OutPortIndex(Projection))
import Luna.Pass.Data.Layer.PortMarker (OutPortRefLike(..))
import Luna.Pass.Data.Layer.NodeMeta (NodeMetaLike(..))
import qualified Luna.Pass.Data.Layer.NodeMeta as NM

import LunaStudio.Data.PortRef (OutPortRef(..))
import LunaStudio.Data.NodeMeta (NodeMeta(..))
import LunaStudio.Data.Position (Position(..))
import LunaStudio.Data.Vector2 (Vector2(..))

import Control.Lens ((?=), (.=), (%=), to, makeWrapped, makePrisms, use, preuse,
                     _Just, (?~), zoom, mapMOf)
import Prologue as X hiding (TypeRep, head, tail, init, last, return, liftIO, fromMaybe, fromJust, when, mapM, mapM_, minimum)
import Control.Monad       as X (return, when, mapM, mapM_, forM)
import Control.Monad.Trans as X (liftIO)
import Data.List           as X (head, tail, init, last, sort, minimum)
import Data.Maybe          as X (fromJust, fromMaybe)

infixr 0 <?!>
(<?!>) :: (Exception e, MonadThrow m) => m (Maybe a) -> e -> m a
m <?!> e = m >>= maybe (throwM e) pure


nameToString :: IR.Name -> String
nameToString = convertTo @String

nameToText :: IR.Name -> Text
nameToText = convert . convertTo @String

stringToName :: String -> IR.Name
stringToName = convert

putLayer :: forall layer t layout m. Layer.Writer t layer m => t layout -> Layer.Data layer layout -> m ()
putLayer = Layer.write @layer

getLayer :: forall layer t layout m. Layer.Reader t layer m => t layout -> m (Layer.Data layer layout)
getLayer = Layer.read @layer

modifyLayer_ :: forall layer t layout m. (Layer.Reader t layer m, Layer.Writer t layer m) => t layout -> (Layer.Data layer layout -> Layer.Data layer layout) -> m ()
modifyLayer_ comp f = getLayer @layer comp >>= putLayer @layer comp . f


type AnyExpr = Terms
type AnyExprLink = Links
type Expr = Term
type SubPass = Pass
type Link a b = Edge.Edge (a *-* b)
type SomeExpr = IR.SomeTerm
type LeftSpacedSpan a = Span.LeftSpacedSpan
type IRSuccs = IR.Users

pattern LeftSpacedSpan a <- Span.LeftSpacedSpan a where
    LeftSpacedSpan a = Span.LeftSpacedSpan a

type MarkedExprMap = TermMap
pattern MarkedExprMap m <- TermMap m where
    MarkedExprMap m = TermMap m

type ASGFunction = IR.Function
type ClsASG = IR.Record

pattern ASGFunction n as b <- IR.UniTermFunction (Ast.Function n as b)
pattern LeftSection f a <- IR.UniTermSectionLeft (Ast.SectionLeft f a)
pattern RightSection f a <- IR.UniTermSectionRight (Ast.SectionRight f a)
pattern Unit n as b <- IR.UniTermUnit (Ast.Unit n as b)
pattern Unify l r <- IR.UniTermUnify (Ast.Unify l r)
pattern Lam i o <- IR.UniTermLam (Ast.Lam i o)
pattern Missing <- IR.UniTermMissing (Ast.Missing)
pattern App f a <- IR.UniTermApp (Ast.App f a)
pattern Cons n a <- IR.UniTermCons (Ast.Cons n a)
pattern Grouped g <- IR.UniTermGrouped (Ast.Grouped g)
pattern Var n <- IR.UniTermVar (Ast.Var n)
pattern Marked m n <- IR.UniTermMarked (Ast.Marked m n)
pattern Marker  l <- IR.UniTermMarker (Ast.Marker l)
pattern List  l <- IR.UniTermList (Ast.List l)
pattern Tuple t <- IR.UniTermTuple (Ast.Tuple t)
pattern Seq l r <- IR.UniTermSeq (Ast.Seq l r)
pattern Blank <- IR.UniTermBlank (Ast.Blank)
pattern Acc n e <- IR.UniTermAcc (Ast.Acc n e)
pattern Documented doc e <- IR.UniTermDocumented (Ast.Documented doc e)
pattern IRString s <- IR.UniTermRawString (Ast.RawString s)
pattern IRNumber a b c <- IR.UniTermNumber (Ast.Number a b c)
pattern ResolvedCons a b c d <- IR.UniTermResolvedCons (Ast.ResolvedCons a b c d)
pattern ResolvedDef a b <- IR.UniTermResolvedDef (Ast.ResolvedDef a b)
pattern ClsASG a b c d e <- IR.UniTermRecord (Ast.Record a b c d e)
pattern Metadata a <- IR.UniTermMetadata (Ast.Metadata a)
pattern ImportHub a <- IR.UniTermImportHub (Ast.ImportHub a)
pattern Import doc e <- IR.UniTermImp (Ast.Imp doc e)
pattern ImportSrc a <- IR.UniTermImportSource (Ast.ImportSource a)
pattern Invalid a <- IR.UniTermInvalid (Ast.Invalid a)

getAttr :: forall attr m. (Monad m, Attr.Getter attr m) => m attr
putAttr :: forall attr m. (Monad m, Attr.Setter attr m) => attr -> m ()
getAttr = Attr.get @attr
putAttr = Attr.put

source :: (Layer.Reader IR.Link IR.Source m, Coercible (Expr t) (Expr (Layout.Get IR.Source layout)))
       => IR.Link layout -> m (Expr t)
source = \a -> IR.source a >>= \b -> return (coerce b)

target :: (Layer.Reader IR.Link IR.Target m, Coercible (Expr t) (Expr (Layout.Get IR.Target layout)))
       => IR.Link layout -> m (Expr t)
target = \a -> IR.target a >>= \b -> return (coerce b)


matchExpr :: forall t layout m a. Layer.Reader t IR.Model m => t layout -> (IR.UniTerm layout -> m a) -> m a
matchExpr e f = Layer.read @IR.Model e >>= f

ptrListToList :: MonadIO m => PtrList.ComponentVector comp layout -> m [Component comp layout]
ptrListToList = PtrList.toList

ociSetToList :: (MonadIO m) => PtrSet.ComponentSet c l -> m [Component c l]
ociSetToList = Mutable.toList 

generalize :: Coercible a b => a -> b
generalize = coerce

irDelete :: (Destruct.Delete m) => Expr a -> m ()
irDelete e = Destruct.delete e

irDeleteLink :: (DestructEdge.Delete m) => Edge.Edge a -> m ()
irDeleteLink e = DestructEdge.delete e

narrowTerm :: forall b m a. Monad m => Expr a -> m (Maybe (Expr b))
narrowTerm e = return $ Just (coerce e)

-- deepDelete :: ( Layer.Reader Node.Node IR.Model m
--           , Layer.IsUnwrapped Node.Uni
--           , Traversal.SubComponents Edge.Edges m (Node.Uni layout)
--           , MonadIO m
--           , Layered.Getter (ByteSize (Component Edge.Edges)) m
--           , Layered.Getter (Layer.DynamicManager Edge.Edges) m
--           , Layered.Getter (MemPool (Component.Some Edge.Edges)) m
--           ) => Node.Node layout -> m ()
deepDelete e = Destruct.deleteSubtree e
deepDeleteWithWhitelist :: forall m layout. Destruct.DeleteSubtree m => Node.Node layout -> Set.Set Node.Some -> m ()
deepDeleteWithWhitelist = flip Destruct.deleteSubtreeWithWhitelist


link :: Construction.Creator m => Term src -> Term tgt -> m (Link src tgt)
link = Construction.new

modifyExprTerm = error "modifyExprTerm"

compListToList :: List.ComponentList a -> [Component.Some a]
compListToList List.Nil = []
compListToList (List.Cons a l) = a : compListToList l

inputs :: Layer.Reader Node.Node IR.Model f => Node.Node layout -> f [Component.Some Edge.Edges]
inputs ref = compListToList <$> IR.inputs ref


fromNodeMeta :: MonadIO m => NodeMeta -> m NodeMetaLike
fromNodeMeta (NodeMeta (Position (Vector2 x y)) d s) = NodeMetaLike (NM.Position x y) d <$> mapMOf (_Just . both) (Foreign.fromList . convert) s

toNodeMeta :: MonadIO m => NodeMetaLike -> m NodeMeta
toNodeMeta (NodeMetaLike (NM.Position x y) d s) = NodeMeta (Position (Vector2 x y)) d <$> mapMOf (_Just . both) (fmap convert . Foreign.toList) s

fromPortMarker :: MonadIO m => OutPortRefLike -> m OutPortRef
fromPortMarker (OutPortRefLike uuid fvec) =
   OutPortRef (convert uuid) <$> (coerce <$> Foreign.toList fvec)

toPortMarker :: MonadIO m => OutPortRef -> m OutPortRefLike
toPortMarker (OutPortRef a b) = OutPortRefLike (convert a) <$> Foreign.fromList (coerce b :: [Int])
