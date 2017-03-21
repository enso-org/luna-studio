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
  , runAliasAnalysis
  , runASTOp
  , match
  ) where

import           Empire.Prelude       hiding (mempty)
import           Prologue             (mempty)

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
import qualified Luna.Syntax.Text.Parser.CodeSpan             as CodeSpan
import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
import qualified Data.SpanTree                                as SpanTree
import           Luna.Syntax.Text.Source                      (Source, SourceTree)

import qualified OCI.IR.Repr.Vis                   as Vis
import qualified Control.Monad.State.Dependent.Old as DepOld

type ASTOp m = (MonadThrow m,
                MonadCatch m,
                MonadPassManager m,
                MonadIO m,
                MonadState Graph m,
                Emitters EmpireEmitters m,
                Editors Net  '[AnyExpr, AnyExprLink] m,
                Editors Attr '[Source, Parser.ParsedModule, SourceTree, MarkedExprMap] m,
                Editors Layer EmpireLayers m,
                DepOld.MonadGet Vis.V Vis.Vis m,
                DepOld.MonadPut Vis.V Vis.Vis m,
                Parser.IRSpanTreeBuilding m)


type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // Succs,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID,
                      AnyExpr // CodeSpan.CodeSpan,
                      AnyExpr // Parser.Parser]

type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink]

data EmpirePass
type instance Abstract   EmpirePass = EmpirePass
type instance Inputs     Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer EmpirePass = EmpireLayers
type instance Inputs     Attr  EmpirePass = '[Source, Parser.ParsedModule, SourceTree, MarkedExprMap] -- Parser attrs temporarily - probably need to call it as a separate Pass
type instance Inputs     Event EmpirePass = '[]

type instance Outputs    Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer EmpirePass = EmpireLayers
type instance Outputs    Attr  EmpirePass = '[Source, Parser.ParsedModule, SourceTree, MarkedExprMap]
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

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
         -> Command Graph a
runASTOp pass = runPass inits pass where
    inits = do
        setAttr (getTypeDesc @SourceTree)           $ (mempty :: SourceTree)
        setAttr (getTypeDesc @MarkedExprMap)        $ (mempty :: MarkedExprMap)
        setAttr (getTypeDesc @Source)               $ (error "Data not provided: Source")
        setAttr (getTypeDesc @Parser.ParsedModule)  $ (error "Data not provided: ParsedModule")


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    --TODO[MK]: AA is broken, fix it and then just pass BH.body here
    items <- uses (Graph.breadcrumbHierarchy . BH.children) toList
    let getPtr it = case it ^. BH.self of
          Just (_, r) -> BH.getAnyRef r
        roots = getPtr <$> items

    let inits = do
            Pass.setAttr (getTypeDesc @UnresolvedVars)   $ UnresolvedVars   []
            Pass.setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []
            Pass.setAttr (getTypeDesc @NegativeConses)   $ NegativeConses   []
            Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots $ map unsafeGeneralize roots
    runPass inits AliasAnalysis.runAliasAnalysis

runPass :: forall b a pass. KnownPass pass
        => Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO)))))) b
        -> Pass.SubPass pass (Pass.PassManager (IRBuilder (Parser.IRSpanTreeBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))))) a
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
