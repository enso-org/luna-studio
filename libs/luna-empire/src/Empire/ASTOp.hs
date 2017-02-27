{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Empire.ASTOp (
    ASTOp
  , EmpirePass
  , runAliasAnalysis
  , runASTOp
  , match
  ) where

import           Empire.Prelude

import           Control.Monad.State  (StateT, runStateT, get, put)
import qualified Data.Map             as Map
import           Empire.Data.Graph    (ASTState(..), Graph, withVis)
import qualified Empire.Data.Graph    as Graph (ast, nodeMapping, getAnyRef)
import           Empire.Data.Layers   (Marker, Meta,
                                      InputsLayer, TypeLayer, TCData)
import           Empire.Empire        (Command)

import           Data.Event           (Emitters, type (//))
import           Data.Graph.Class     (MonadRefLookup(..), Net)
import           Data.TypeDesc        (getTypeDesc)
import           Luna.IR              hiding (get, put, match)
import           Luna.IR.Layer.Succs  (Succs)
import           Luna.Pass            (Inputs, Outputs, Preserves, KnownPass)
import qualified Luna.Pass            as Pass (SubPass, eval')
import qualified Luna.Pass.Manager    as Pass (PassManager, RefCache, get, setAttr)

import           System.Log                                 (Logger, DropLogger, dropLogs)
import           Luna.Passes.Data.ExprRoots                 (ExprRoots(..))
import           Luna.Passes.Resolution.Data.UnresolvedVars (UnresolvedVars(..))
import qualified Luna.Passes.Resolution.AliasAnalysis       as AliasAnalysis
import qualified Luna.Passes.Transform.Parsing.CodeSpan     as CodeSpan
import qualified Luna.Passes.Transform.Parsing.Parser       as Parser
import qualified Data.SpanTree                              as SpanTree

import qualified Luna.IR.Repr.Vis           as Vis

type ASTOp m = (MonadThrow m, MonadPassManager m,
                MonadIO m,
                MonadState Graph m,
                Emitters EmpireEmitters m,
                Editors Net '[AnyExpr, AnyExprLink] m,
                Editors Layer EmpireLayers m,
                Parser.IRSpanTreeBuilding m)


type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // InputsLayer,
                      AnyExpr // Succs,
                      AnyExpr // TCData,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID,
                      AnyExpr // Redirect,
                      AnyExpr // CodeSpan.CodeSpan,
                      AnyExpr // Parser.Parser]

type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink]

data EmpirePass
type instance Abstract   EmpirePass = EmpirePass
type instance Inputs     Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer EmpirePass = EmpireLayers
type instance Inputs     Attr  EmpirePass = '[]
type instance Inputs     Event EmpirePass = '[]

type instance Outputs    Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer EmpirePass = EmpireLayers
type instance Outputs    Attr  EmpirePass = '[]
type instance Outputs    Event EmpirePass = EmpireEmitters

type instance Preserves        EmpirePass = '[]

instance MonadPassManager m => MonadRefLookup Net (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Event (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Layer (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Attr (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

match = matchExpr

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (Pass.RefCache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
         -> Command Graph a
runASTOp pass = runPass (return ()) pass

runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    roots <- uses Graph.nodeMapping $ map Graph.getAnyRef . Map.elems
    let inits = do
            Pass.setAttr (getTypeDesc @UnresolvedVars) $ UnresolvedVars
            Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots $ map unsafeGeneralize roots
    runPass inits AliasAnalysis.runAliasAnalysis

runPass :: forall b a pass. KnownPass pass
        => Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (Pass.RefCache (Logger DropLogger (Vis.VisStateT (StateT Graph IO)))))) b
        -> Pass.SubPass pass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (Pass.RefCache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
        -> Command Graph a
runPass inits pass = do
    g <- get
    ASTState currentStateIR currentStatePass <- use Graph.ast
    let evalIR = flip runStateT g
               . withVis
               . dropLogs
               . runRefCache
               . (\a -> SpanTree.runTreeBuilder a >>= \(foo, _) -> return foo)
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
        inits
        a      <- Pass.eval' @pass pass
        Pass.eval' @EmpirePass $ Vis.snapshot "foo"
        st     <- snapshot
        passSt <- Pass.get
        return (a, (st, passSt))
    put $ newG & Graph.ast .~ ASTState st passSt
    return a
