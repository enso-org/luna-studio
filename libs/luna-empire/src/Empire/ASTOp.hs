{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Empire.ASTOp (
    ASTOp
  , EmpirePass
  , PMStack
  , putNewIR
  , runAliasAnalysis
  , runASTOp
  , runPass
  , runPM
  , runTypecheck
  , match
  ) where

import           Empire.Prelude       hiding (mempty, toList)
import           Prologue             (Text, mempty, toListOf)

import           Control.Monad.Catch  (MonadCatch(..))
import           Control.Monad.State  (MonadState, StateT, evalStateT, runStateT, get, gets, put)
import qualified Control.Monad.State.Dependent as DepState
import qualified Data.Map             as Map
import           Data.Foldable        (toList)
import           Empire.Data.Graph    (ASTState(..), Graph, withVis)
import qualified Empire.Data.Graph    as Graph (ast, breadcrumbHierarchy, pmState, ir, defaultAST)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, attachEmpireLayers)
import           Empire.Empire        (Command)

import           Data.Event           (Emitters, type (//))
import           Data.Graph.Class     (MonadRefLookup(..), Net)
import           Data.TypeDesc        (getTypeDesc)
import           Luna.IR              hiding (get, put, match)
import           Luna.IR.Layer.Succs  (Succs)
import           OCI.Pass.Class       (Inputs, Outputs, Preserves, KnownPass)
import           OCI.IR.Class         (Import)
import qualified OCI.Pass.Class       as Pass (SubPass, eval')
import qualified OCI.Pass.Manager     as Pass (PassManager, Cache, setAttr, State)

import           System.Log                                   (Logger, DropLogger, dropLogs)
import           Luna.Pass.Data.ExprRoots                     (ExprRoots(..))
import qualified Luna.Pass.Transform.Desugaring.PatternTransformation as PatternTransformation
import           Luna.Pass.Resolution.Data.UnresolvedVars     (UnresolvedVars(..))
import           Luna.Pass.Resolution.Data.UnresolvedConses   (UnresolvedConses(..), NegativeConses(..))
import qualified Luna.Pass.Resolution.AliasAnalysis           as AliasAnalysis
import           Luna.Syntax.Text.Parser.Errors (Invalids)
import qualified Luna.Syntax.Text.Parser.Parser               as Parser
import qualified Luna.Syntax.Text.Parser.Parsing              as Parsing
import qualified Luna.Syntax.Text.Parser.CodeSpan             as CodeSpan
import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
import qualified Data.SpanTree                                as SpanTree
import           Luna.Syntax.Text.Source                      (Source, SourceTree)
import qualified Luna.Pass.Typechecking.Typecheck             as Typecheck

import qualified OCI.IR.Repr.Vis                   as Vis
import qualified Control.Monad.State.Dependent.Old as DepOld
import           Luna.Pass.Data.ExprMapping
import           Luna.Builtin.Data.Module          (Imports (..))
import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget (TgtNone))

import           GHC.Stack

type PMStack m = PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger m)))

runPM :: MonadIO m => PMStack m a -> m a
runPM = dropLogs . DepState.evalDefStateT @Cache . evalIRBuilder' . evalPassManager'

type ASTOp m = (MonadThrow m,
                MonadCatch m,
                MonadPassManager m,
                MonadIO m,
                MonadState Graph m,
                Emitters EmpireEmitters m,
                Editors Net  '[AnyExpr, AnyExprLink] m,
                Editors Attr '[Source, Parser.ParsedExpr, SourceTree, MarkedExprMap, Invalids] m,
                Editors Layer EmpireLayers m,
                DepOld.MonadGet Vis.V Vis.Vis m,
                DepOld.MonadPut Vis.V Vis.Vis m,
                Parser.IRSpanTreeBuildingR m,
                HasCallStack)


type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // Succs,
                      AnyExpr // Errors,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID,
                      AnyExpr // CodeSpan.CodeSpan]

type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink,
                        OnDeepDelete // AnyExpr]

data EmpirePass
type instance Abstract   EmpirePass = EmpirePass
type instance Inputs     Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer EmpirePass = EmpireLayers
type instance Inputs     Attr  EmpirePass = '[Source, Parser.ParsedExpr, SourceTree, MarkedExprMap, ExprMapping, Invalids] -- Parser attrs temporarily - probably need to call it as a separate Pass
type instance Inputs     Event EmpirePass = '[]

type instance Outputs    Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer EmpirePass = EmpireLayers
type instance Outputs    Attr  EmpirePass = '[Source, Parser.ParsedExpr, SourceTree, MarkedExprMap, Invalids]
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

deriving instance MonadCatch m => MonadCatch (Pass.PassManager m)
deriving instance MonadCatch m => MonadCatch (DepState.StateT s m)
deriving instance MonadCatch m => MonadCatch (SpanTree.TreeBuilder k t m)

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilderR (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
         -> Command Graph a
runASTOp pass = runPass inits pass where
    inits = do
        setAttr (getTypeDesc @SourceTree)        $ (mempty :: SourceTree)
        setAttr (getTypeDesc @MarkedExprMap)     $ (mempty :: MarkedExprMap)
        setAttr (getTypeDesc @Invalids)          $ (mempty :: Invalids)
        setAttr (getTypeDesc @Source)            $ (error "Data not provided: Source")
        setAttr (getTypeDesc @Parser.ParsedExpr) $ (error "Data not provided: ParsedExpr")


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    roots <- gets $ toListOf $ Graph.breadcrumbHierarchy . BH.body
    let inits = do
            Pass.setAttr (getTypeDesc @UnresolvedVars)   $ UnresolvedVars   []
            Pass.setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []
            Pass.setAttr (getTypeDesc @NegativeConses)   $ NegativeConses   []
            Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots $ map unsafeGeneralize roots
    runPass inits PatternTransformation.runPatternTransformation
    runPass inits AliasAnalysis.runAliasAnalysis

runTypecheck :: Imports -> Command Graph ()
runTypecheck imports = do
    g <- get
    ASTState currentStateIR currentStatePass <- use Graph.ast
    root <- preuse $ Graph.breadcrumbHierarchy . BH.body
    let evalIR = flip runStateT g
               . withVis
               . dropLogs
               . DepState.evalDefStateT @Cache
               . (\a -> SpanTree.runTreeBuilder a >>= \(foo, _) -> return foo)
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((st, passSt), newG) <- liftIO $ evalIR $ do
        Typecheck.typecheck TgtNone imports $ map unsafeGeneralize $ toList root
        st     <- snapshot
        passSt <- DepState.get @Pass.State
        return (st, passSt)
    put $ newG & Graph.ast .~ ASTState st passSt

putNewIR :: IR -> Command Graph ()
putNewIR ir = do
    newAST <- liftIO $ Graph.defaultAST
    Graph.ast .= (newAST & Graph.ir .~ ir)


runPass :: forall pass b a. KnownPass pass
        => Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilderR (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO)))))) b
        -> Pass.SubPass pass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilderR (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
        -> Command Graph a
runPass inits pass = do
    g <- get
    ASTState currentStateIR currentStatePass <- use Graph.ast
    let evalIR = flip runStateT g
               . withVis
               . dropLogs
               . DepState.evalDefStateT @Cache
               . (\a -> SpanTree.runTreeBuilder a >>= \(foo, _) -> return foo)
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
        inits
        a      <- Pass.eval' @pass pass
        st     <- snapshot
        passSt <- DepState.get @Pass.State
        return (a, (st, passSt))
    put $ newG & Graph.ast .~ ASTState st passSt

    return a
