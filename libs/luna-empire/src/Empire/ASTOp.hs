{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}

module Empire.ASTOp (
    ASTOp
  , ClassOp
  , GraphOp
  , ASTOpReq
  , EmpirePass
  , PMStack
  , putNewIR
  , putNewIRCls
  , runAliasAnalysis
  , runASTOp
  , runPass
  , runPM
  , runTypecheck
  , runModuleTypecheck
  , match
  ) where

import           Empire.Prelude       hiding (Type, mempty, toList)
import           Prologue             (mempty)

import           Control.Monad.Catch  (MonadCatch(..))
import           Control.Monad.State  (MonadState, StateT, runStateT, get, put)
import qualified Control.Monad.State.Dependent as DepState
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Empire.Data.Graph    (AST(..), ClsGraph, Graph, withVis)
import qualified Empire.Data.Graph    as Graph
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, SpanLength, SpanOffset)
import           Empire.Empire        (Command)

import           Data.Event           (Emitters, type (//))
import           Data.Graph.Class     (MonadRefLookup(..), Net)
import           Data.TypeDesc        (getTypeDesc)
import           Luna.IR              as IR hiding (Marker, match)
import           Luna.IR.Layer.Succs  (Succs)
import qualified Luna.IR.Term.Unit    as Term
import           OCI.IR.Layout.Typed  (type (>>))
import           OCI.Pass.Class       (Inputs, Outputs, Preserves, KnownPass)
import           OCI.IR.Class         (Import)
import qualified OCI.Pass.Class       as Pass (SubPass, eval')
import qualified OCI.Pass.Manager     as Pass (PassManager, setAttr, State)

import           System.Log                                   (Logger, DropLogger, dropLogs)
import           Luna.Pass.Data.ExprRoots                     (ExprRoots(..))
import qualified Empire.Pass.PatternTransformation            as PatternTransformation
import           Luna.Pass.Resolution.Data.UnresolvedVars     (UnresolvedVars(..))
import           Luna.Pass.Resolution.Data.UnresolvedConses   (UnresolvedConses(..), NegativeConses(..))
import qualified Luna.Pass.Resolution.AliasAnalysis           as AliasAnalysis
import           Luna.Syntax.Text.Parser.Errors (Invalids)
import qualified Luna.Syntax.Text.Parser.Parser               as Parser
import qualified Luna.Syntax.Text.Parser.CodeSpan             as CodeSpan
import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
import           Luna.Syntax.Text.Source                      (Source)
import qualified Luna.Pass.Typechecking.Typecheck             as Typecheck
import qualified Luna.Compilation                             as Compilation
import           Luna.Compilation                             (CompiledModules (..))

import qualified OCI.IR.Repr.Vis                   as Vis
import qualified Control.Monad.State.Dependent.Old as DepOld
import           Luna.Pass.Data.UniqueNameGen               (initNameGen)
import           Luna.Pass.Data.ExprMapping
import           Luna.Builtin.Data.Module          (Imports (..), unionsImports)
import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget (TgtNone))
import qualified Luna.Pass.UnitCompilation.ModuleProcessing as ModuleTC
import qualified Luna.Pass.Sourcing.UnitLoader              as UnitLoader
import           Luna.IR.Term.World                         (WorldExpr)
import           Luna.IR.Term.Unit                          (UnitSet)


type PMStack m = PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger m)))

runPM :: MonadIO m => PMStack m a -> m a
runPM = dropLogs . DepState.evalDefStateT @Cache . evalIRBuilder' . evalPassManager'

type ASTOpReq a m = (MonadThrow m,
                     MonadCatch m,
                     MonadPassManager m,
                     MonadIO m,
                     MonadState a m,
                     Emitters EmpireEmitters m,
                     Editors Net  '[AnyExpr, AnyExprLink] m,
                     Editors Attr '[Source, Parser.ParsedExpr, MarkedExprMap, Invalids] m,
                     Editors Layer EmpireLayers m,
                     DepOld.MonadGet Vis.V Vis.Vis m,
                     DepOld.MonadPut Vis.V Vis.Vis m)

type ASTOp a m = (ASTOpReq a m, HasCallStack)
type GraphOp m = ASTOp Graph m
type ClassOp m = ASTOp ClsGraph m


type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // Succs,
                      AnyExpr // Errors,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID,
                      AnyExpr // SpanLength,
                      AnyExprLink // SpanOffset,
                      AnyExpr // CodeSpan.CodeSpan]

type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink,
                        OnDeepDelete // AnyExpr]

data EmpirePass
type instance Abstract   EmpirePass = EmpirePass
type instance Inputs     Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer EmpirePass = EmpireLayers
type instance Inputs     Attr  EmpirePass = '[Source, Parser.ParsedExpr, MarkedExprMap, ExprMapping, Invalids] -- Parser attrs temporarily - probably need to call it as a separate Pass
type instance Inputs     Event EmpirePass = '[]

type instance Outputs    Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer EmpirePass = EmpireLayers
type instance Outputs    Attr  EmpirePass = '[Source, Parser.ParsedExpr, MarkedExprMap, Invalids]
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

class GraphRunner g where
    runPass :: forall pass b a. KnownPass pass
            => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO))))) b
            -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO)))))) a
            -> Command g a

instance GraphRunner Graph where
    runPass :: forall pass b a. KnownPass pass
            => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))) b
            -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO)))))) a
            -> Command Graph a
    runPass inits pass = do
        g <- get
        AST currentStateIR currentStatePass <- use Graph.ast
        let evalIR = flip runStateT g
                   . withVis
                   . dropLogs
                   . DepState.evalDefStateT @Cache
                   . flip evalIRBuilder currentStateIR
                   . flip evalPassManager currentStatePass
        ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
            inits
            a      <- Pass.eval' @pass pass
            st     <- snapshot
            passSt <- DepState.get @Pass.State
            return (a, (st, passSt))
        put $ newG & Graph.ast .~ AST st passSt

        return a

instance GraphRunner ClsGraph where
    runPass :: forall pass b a. KnownPass pass
            => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))) b
            -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO)))))) a
            -> Command ClsGraph a
    runPass inits pass = do
        g <- get
        AST currentStateIR currentStatePass <- use Graph.clsAst
        let evalIR = flip runStateT g
                   . withVis
                   . dropLogs
                   . DepState.evalDefStateT @Cache
                   . flip evalIRBuilder currentStateIR
                   . flip evalPassManager currentStatePass
        ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
            inits
            a      <- Pass.eval' @pass pass
            st     <- snapshot
            passSt <- DepState.get @Pass.State
            return (a, (st, passSt))
        put $ newG & Graph.clsAst .~ AST st passSt

        return a

runASTOp :: GraphRunner g
         => Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO)))))) a
         -> Command g a
runASTOp pass = runPass inits pass where
    inits = do
        setAttr (getTypeDesc @MarkedExprMap)     $ (mempty :: MarkedExprMap)
        setAttr (getTypeDesc @Invalids)          $ (mempty :: Invalids)
        setAttr (getTypeDesc @Source)            $ (error "Data not provided: Source")
        setAttr (getTypeDesc @Parser.ParsedExpr) $ (error "Data not provided: ParsedExpr")


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    root <- use $ Graph.breadcrumbHierarchy . BH.self
    let inits = do
            Pass.setAttr (getTypeDesc @UnresolvedVars)   $ UnresolvedVars   []
            Pass.setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []
            Pass.setAttr (getTypeDesc @NegativeConses)   $ NegativeConses   []
            Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots [unsafeGeneralize root]
    runPass inits PatternTransformation.runPatternTransformation
    runPass inits AliasAnalysis.runAliasAnalysis

runTypecheck :: Imports -> Command Graph ()
runTypecheck imports = do
    g <- get
    AST currentStateIR currentStatePass <- use Graph.ast
    root <- use $ Graph.breadcrumbHierarchy . BH.self
    let evalIR = flip runStateT g
               . withVis
               . dropLogs
               . DepState.evalDefStateT @Cache
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((st, passSt), newG) <- liftIO $ evalIR $ do
        Typecheck.typecheck TgtNone imports [unsafeGeneralize root]
        st     <- snapshot
        passSt <- DepState.get @Pass.State
        return (st, passSt)
    put $ newG & Graph.ast .~ AST st passSt

runModuleTypecheck :: Map.Map Name FilePath -> CompiledModules -> Command ClsGraph (Either Compilation.ModuleCompilationError (Imports, CompiledModules))
runModuleTypecheck sources cmpMods@(CompiledModules _ prims) = do
    unit :: Expr Unit <- uses Graph.clsClass unsafeGeneralize
    g <- get
    AST ir pmState <- use Graph.clsAst
    let evalIR = flip runStateT g
               . withVis
               . dropLogs
               . DepState.evalDefStateT @Cache
               . flip evalIRBuilder ir
               . flip evalPassManager pmState
    (res, newG) <- liftIO $ evalIR $ do
        Pass.setAttr (getTypeDesc @WorldExpr)                 $ error "Data not provided: WorldExpr"
        Pass.setAttr (getTypeDesc @UnitLoader.UnitsToLoad)    $ error "Data not provided: UnitsToLoad"
        Pass.setAttr (getTypeDesc @UnitLoader.SourcesManager) $ error "Data not provided: SourcesManager"
        Pass.setAttr (getTypeDesc @UnitSet)                   $ error "Data not provided: UnitSet"
        Pass.setAttr (getTypeDesc @Invalids)                  $ (mempty :: Invalids)
        initNameGen
        impNames <- Pass.eval' $ do
            imphub   <- unit @^. Term.imports
            imps     <- readWrappedSources (unsafeGeneralize imphub :: Expr (UnresolvedImportHub >> UnresolvedImport >> UnresolvedImportSrc))
            impNames <- for imps $ \imp -> do
                src <- imp @^. Term.termUnresolvedImport_source
                Term.Absolute path <- src @. wrapped
                return path
            cls <- IR.matchExpr unit $ \case
                IR.Unit _ _ c -> IR.source c
            UnitLoader.partitionASGCls $ IR.unsafeGeneralize cls
            return impNames
        let impNamesWithBase = Set.toList $ Set.insert "Std.Base" $ Set.fromList impNames
        result <- liftIO $ Compilation.requestModules sources impNamesWithBase cmpMods
        case result of
            Right (imports, newCmpMods) -> do
                let imps = unionsImports $ prims : Map.elems imports
                res <- ModuleTC.processModule imps "<<interactive>>" (IR.unsafeGeneralize unit)
                return $ Right (res, newCmpMods)
            Left err -> return $ Left err
    return res

putNewIR :: IR -> Command Graph ()
putNewIR ir = do
    pmState <- liftIO $ Graph.defaultPMState
    Graph.ast .= AST ir pmState

putNewIRCls :: IR -> Command ClsGraph ()
putNewIRCls ir = do
    newAST <- liftIO $ Graph.emptyClsAST
    Graph.clsAst .= (newAST & Graph.ir .~ ir)


-- runPass :: forall pass g b a. KnownPass pass
--         => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO))))) b
--         -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO)))))) a
--         -> Command g a
-- runPass inits pass = do
--     g <- get
--     AST currentStateIR currentStatePass <- use Graph.ast
--     let evalIR = flip runStateT g
--                . withVis
--                . dropLogs
--                . DepState.evalDefStateT @Cache
--                . flip evalIRBuilder currentStateIR
--                . flip evalPassManager currentStatePass
--     ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
--         inits
--         a      <- Pass.eval' @pass pass
--         st     <- snapshot
--         passSt <- DepState.get @Pass.State
--         return (a, (st, passSt))
--     put $ newG & Graph.ast .~ AST st passSt
--
--     return a
