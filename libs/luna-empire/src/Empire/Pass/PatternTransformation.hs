{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Empire.Pass.PatternTransformation where

import           OCI.Pass        (SubPass, Pass, Inputs, Outputs, Preserves)

import Empire.Prelude hiding (List, Type, String, s, new, cons)
import qualified Luna.Prelude as P hiding (List)
import qualified OCI.IR.Repr.Vis as Vis
import Data.TypeDesc
import OCI.IR.Combinators
import Luna.IR hiding (expr)
import Luna.Pass.Data.ExprRoots
import OCI.Pass.Manager
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, attachEmpireLayers, SpanLength, SpanOffset)
import           Data.Text.Position      (Delta)
import           Data.Text.Span          (LeftSpacedSpan(..), SpacedSpan(..), leftSpacedSpan)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan, realSpan)
import Data.TypeDesc

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Data.Map   (Map)

import System.Log

data PatternTransformation
type instance Abstract         PatternTransformation = PatternTransformation
type instance Inputs     Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer PatternTransformation = '[AnyExpr // Model, AnyExpr // CodeSpan, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // SpanLength, AnyExprLink // SpanOffset]
type instance Inputs     Attr  PatternTransformation = '[ExprRoots]
type instance Inputs     Event PatternTransformation = '[]

type instance Outputs    Net   PatternTransformation = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer PatternTransformation = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs, AnyExpr // SpanLength, AnyExprLink // SpanOffset]
type instance Outputs    Attr  PatternTransformation = '[]
type instance Outputs    Event PatternTransformation = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink, OnDeepDelete // AnyExpr]
type instance Preserves        PatternTransformation = '[]

runPatternTransformation :: (MonadRef m, MonadPassManager m, MonadThrow m) => Pass PatternTransformation m
runPatternTransformation = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ transformPatterns roots

data ParseError = ParseError P.String
    deriving (Show)

instance Exception ParseError where
    displayException (ParseError s) = "Parse error: " <> s

dumpConsApplication :: (MonadRef m, MonadPassManager m, MonadThrow m) => Expr Draft -> SubPass PatternTransformation m (Name, [Expr Draft], [Link (Expr Draft) (Expr Draft)])
dumpConsApplication expr = matchExpr expr $ \case
    Grouped g -> dumpConsApplication =<< source g
    Cons n _  -> return (n, [], [])
    App f a   -> do
        (n, args, links) <- dumpConsApplication =<< source f
        arg <- source a
        return (n, arg : args, a:links)
    _         -> throwM $ ParseError "Invalid pattern match in code"

flattenPattern :: (MonadRef m, MonadPassManager m, MonadThrow m) => Expr Draft -> SubPass PatternTransformation m (Expr Draft)
flattenPattern expr = matchExpr expr $ \case
    Grouped g -> do
        a <- flattenPattern =<< source g
        putLayer @SpanLength a =<< getLayer @SpanLength expr
        return a
    Var{}     -> return expr
    Cons{}    -> return expr
    List as   -> do
        as' <- mapM (flattenPattern <=< source) as
        l   <- generalize <$> list as'
        putLayer @SpanLength l =<< getLayer @SpanLength expr
        childLinks <- inputs l
        P.forM (zip as childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        return l
    Tuple as   -> do
        as' <- mapM (flattenPattern <=< source) as
        t   <- generalize <$> tuple as'
        putLayer @SpanLength t =<< getLayer @SpanLength expr
        childLinks <- inputs t
        P.forM (zip as childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        return t
    App{}     -> do
        (name, children, links) <- dumpConsApplication expr
        flatChildren            <- mapM flattenPattern $ reverse children
        res                     <- generalize <$> cons name flatChildren
        childLinks              <- inputs res
        P.forM (zip (reverse links) childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        LeftSpacedSpan (SpacedSpan _off len) <- fmap (view CodeSpan.realSpan) $ getLayer @CodeSpan expr
        putLayer @SpanLength res len
        return res
    _         -> return expr

transformPatterns :: (MonadRef m, MonadPassManager m, MonadThrow m) => Expr Draft -> SubPass PatternTransformation m ()
transformPatterns expr = matchExpr expr $ \case
    Lam i o   -> do
        inp <- source i
        res <- flattenPattern inp
        replace res inp
        transformPatterns =<< source o
    Unify l r -> do
        left <- source l
        res  <- flattenPattern left
        replace res left
        transformPatterns =<< source r
    ASGFunction n as b -> do
        P.forM_ as $ \a' -> do
            a   <- source a'
            res <- flattenPattern a
            replace res a
        transformPatterns =<< source b
    _ -> mapM_ (transformPatterns <=< source) =<< inputs expr
