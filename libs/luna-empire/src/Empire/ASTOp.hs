{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE UndecidableInstances       #-}

module Empire.ASTOp (
    ASTOp
  , ClassOp
  , GraphOp
  , EmpirePass
  , Printer
  , defaultClsGraph
  , runAliasAnalysis
  , runASTOp
  , liftScheduler
  , match
  ) where

import           Empire.Prelude              hiding (Type, toList)

import           Control.Monad.Catch         (MonadCatch(..))
import           Control.Monad.State         (MonadState, StateT(..), runStateT)
import qualified Control.Monad.State.Layered as Layered
import qualified Data.TypeMap.MultiState     as MultiState
import           Data.IORef
import           Data.Graph.Component.Edge.Class (Edges)
import           Data.Graph.Component.Node.Class (Nodes)
import           Luna.Pass.Data.Layer.NodeMeta   (Meta)
import           Luna.Pass.Data.Layer.PortMarker (PortMarker)
import           Luna.Pass.Data.Layer.SpanLength (SpanLength)
import           Luna.Pass.Data.Layer.SpanOffset (SpanOffset)
import           Data.Graph.Data.Component.Class (Component)
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Graph.Class as LunaGraph
import qualified Data.Graph.Data.Layer.Class as Layer
import           Empire.Data.Graph           (ClsGraph, Graph, pmState,
                                              pmScheduler, pmStage, userState)
import qualified Empire.Data.Graph           as Graph
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers          ()
import           Empire.Empire               (Command)
import qualified Empire.Pass.PatternTransformation as PatternTransformation
import           Foreign.Info.ByteSize       (ByteSize)
import           Foreign.Memory.Pool         (MemPool)
import           Luna.IR                     as IR hiding (Unit, Marker,
                                             match, source)
import qualified Luna.Pass                   as Pass
import qualified Luna.Pass.Data.Stage        as TC
import           Luna.Pass.Data.Root         (Root(..))
import qualified Luna.Pass.Resolve.AliasAnalysis as AliasAnalysis
import           Luna.Pass.Resolve.Data.UnresolvedVariables (UnresolvedVariables(..))
import           Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import           OCI.Pass.Definition.Class   (Pass(..))
import qualified OCI.Pass.Management.Scheduler as Scheduler


type Printer a m = (MonadIO m,
                     MonadState a m,
                     Layer.Reader (Component Nodes) IR.Model m,
                     Layer.Reader (Component Nodes) PortMarker m,
                     Layer.Reader (Component Nodes) IR.Users m,
                     Layer.Reader (Component Nodes) CodeSpan m,
                     Layer.Reader (Component Nodes) SpanLength m,
                     Layer.Reader (Component Nodes) Meta m,
                     Layer.Reader (Component Nodes) Type m,
                     Layer.Reader (Component Edges) SpanOffset m,
                     Layer.Reader IR.Link IR.Source m,
                     Layer.Reader IR.Link IR.Target m,
                     Layered.Getter (ByteSize (Component Nodes)) m,
                     Layered.Getter (ByteSize (Component Edges)) m,
                     Layered.Getter (Layer.DynamicManager Edges) m,
                     Layered.Getter (Layer.DynamicManager Nodes) m,
                     Layered.Getter (MemPool (Component.Some Nodes)) m,
                     Layered.Getter (MemPool (Component.Some Edges)) m,
                     MonadCatch m)

type instance LunaGraph.Discover (StateT s m) = LunaGraph.Discover m
type instance LunaGraph.Discover (Layered.StateT s m) = LunaGraph.Discover m


type ASTOp g = StateT g (Pass.Pass TC.Stage EmpirePass)
type GraphOp = ASTOp Graph
type ClassOp = ASTOp ClsGraph

data EmpirePass
type instance Pass.Spec EmpirePass t = EmpirePassSpec t
type family EmpirePassSpec t where
    EmpirePassSpec (Pass.In  Pass.Attrs)  = '[]
    EmpirePassSpec (Pass.Out Pass.Attrs)  = '[]
    EmpirePassSpec (Pass.In  AnyExpr)     = '[Model, Type, Users, SpanLength, CodeSpan, Meta, PortMarker]
    EmpirePassSpec (Pass.Out AnyExpr)     = '[Model, Type, Users, SpanLength, CodeSpan, Meta, PortMarker]
    EmpirePassSpec (Pass.In  AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec (Pass.Out AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec t                      = Pass.BasicPassSpec t


match :: (Monad m, Layer.Reader t Model m) => t layout -> (UniTerm layout -> m a) -> m a
match = matchExpr

deriving instance MonadCatch (Pass stage t)
deriving instance MonadCatch m => MonadCatch (MultiState.MultiStateT s m)
deriving instance MonadCatch m => MonadCatch (LunaGraph.GraphT s m)


defaultClsAST :: IO (IR.SomeTerm, LunaGraph.State TC.Stage, Scheduler.State)
defaultClsAST = do
    ((a, scState), grState) <- LunaGraph.encodeAndEval @TC.Stage $ do
        foo <- Scheduler.runT $ do
            m <- liftIO $ newIORef (error "emptyreturn")
            Scheduler.registerPassFromFunction__ @TC.Stage @EmpirePass $ do
                a <- do
                    hub <- IR.importHub []
                    c <- IR.record False "A" [] [] []
                    IR.unit hub [] c
                liftIO $ writeIORef m $ generalize a
            Scheduler.runPassByType @EmpirePass
            foo <- liftIO $ readIORef m
            return foo
        st <- LunaGraph.getState
        return (foo, st)
    return (a, grState, scState)


defaultClsGraph :: IO ClsGraph
defaultClsGraph = do
    (ast, _scSt, _grSt) <- defaultClsAST
    let cls = Graph.ClsGraph ast def def def def def
    return cls


runASTOp :: forall a g.
            StateT g (Pass.Pass TC.Stage EmpirePass) a
         -> Command g a
runASTOp p = runPass (return ()) p

runPass :: forall pass a b g. (Scheduler.PassRegister TC.Stage pass TC.Monad)
         => TC.Monad b
         -> StateT g (Pass.Pass TC.Stage pass) a
         -> Command g a
runPass inits p = do
    g <- use userState
    m <- liftIO $ newIORef (error "emptyreturn")

    (ret, g') <- liftScheduler $ do
        inits
        Scheduler.registerPassFromFunction__ @TC.Stage @pass $ do
            (a, g') <- runStateT p g
            liftIO $ writeIORef m (a, g')
        Scheduler.runPassByType @pass
        (a, g') <- liftIO $ readIORef m
        return (a, g')
    userState .= g'
    return ret

liftScheduler :: Scheduler.SchedulerT (LunaGraph.GraphT TC.Stage IO) b
              -> Command g b
liftScheduler act = do
    schState <- use $ pmState.pmScheduler
    graphState <- use $ pmState.pmStage
    ((b, newGraphState), newSchState) <- liftIO $ flip LunaGraph.eval graphState $ flip Layered.runT schState $ unwrap $ do
        b <- act
        newGraphState <- LunaGraph.getState
        return (b, newGraphState)
    pmState.pmStage .= newGraphState
    pmState.pmScheduler .= newSchState
    return b


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    root <- use $ userState . Graph.breadcrumbHierarchy . BH.self
    let inits = do
            Scheduler.registerAttr @Root
            Scheduler.enableAttrByType @Root
            Scheduler.setAttr @Root $ Root root
            Scheduler.registerAttr @PatternTransformation.ExprRoots
            Scheduler.enableAttrByType @PatternTransformation.ExprRoots
            Scheduler.setAttr @PatternTransformation.ExprRoots $ PatternTransformation.ExprRoots [coerce root]
            Scheduler.registerAttr @UnresolvedVariables
            Scheduler.enableAttrByType @UnresolvedVariables
    runPass @PatternTransformation.PatternTransformation inits $ StateT $ \a -> PatternTransformation.runPatternTransformation >> return ((),a)
    runPass @AliasAnalysis.AliasAnalysis inits $ StateT $ \a -> Pass.definition @TC.Stage @AliasAnalysis.AliasAnalysis >> return ((), a)


-- runTypecheck :: CurrentTarget -> Imports -> Command Graph ()
-- runTypecheck currentTarget imports = do
--     g <- get
--     AST currentStateIR currentStatePass <- use Graph.ast
--     root <- use $ Graph.breadcrumbHierarchy . BH.self
--     let evalIR = flip runStateT g
--                . withVis
--                . dropLogs
--                . DepState.evalDefStateT @Cache
--                . flip evalIRBuilder currentStateIR
--                . flip evalPassManager currentStatePass
--     ((st, passSt), newG) <- liftIO $ evalIR $ do
--         Typecheck.typecheck currentTarget imports [unsafeGeneralize root]
--         st     <- snapshot
--         passSt <- DepState.get @Pass.State
--         return (st, passSt)
--     put $ newG & Graph.ast .~ AST st passSt

-- evalTC :: ClsGraph
--        -> IR
--        -> State (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))))
--        -> Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))) a
--        -> IO (a, ClsGraph)
-- evalTC g ir pmState = flip runStateT g
--                     . withVis
--                     . dropLogs
--                     . DepState.evalDefStateT @Cache
--                     . flip evalIRBuilder ir
--                     . flip evalPassManager pmState

-- tcInit :: MonadPassManager m => m ()
-- tcInit = do
--     Pass.setAttr (getTypeDesc @WorldExpr)                 $ error "Data not provided: WorldExpr"
--     Pass.setAttr (getTypeDesc @UnitLoader.UnitsToLoad)    $ error "Data not provided: UnitsToLoad"
--     Pass.setAttr (getTypeDesc @UnitLoader.SourcesManager) $ error "Data not provided: SourcesManager"
--     Pass.setAttr (getTypeDesc @UnitSet)                   $ error "Data not provided: UnitSet"
--     Pass.setAttr (getTypeDesc @Invalids)                  $ (mempty :: Invalids)
--     initNameGen

-- extractImportedModules :: (MonadPassManager m, MonadException PassEvalError m) => Expr Unit -> m (Set.Set QualName)
-- extractImportedModules unit = do
--     impNames <- Pass.eval' $ do
--         imphub   <- unit @^. Term.imports
--         imps     <- readWrappedSources (unsafeGeneralize imphub :: Expr (UnresolvedImportHub >> UnresolvedImport >> UnresolvedImportSrc))
--         impNames <- for imps $ \imp -> do
--             src <- imp @^. Term.termUnresolvedImport_source
--             Term.Absolute path <- src @. wrapped
--             return path
--         cls <- IR.matchExpr unit $ \case
--             IR.Unit _ _ c -> IR.source c
--         UnitLoader.partitionASGCls $ IR.unsafeGeneralize cls
--         return impNames
--     let impNamesWithBase = Set.insert "Std.Base" $ Set.fromList impNames
--     return impNamesWithBase

-- getImportedModules :: Command ClsGraph (Set.Set QualName)
-- getImportedModules = do
--     unit :: Expr Unit <- uses Graph.clsClass unsafeGeneralize
--     g <- get
--     AST ir pmState <- use Graph.clsAst
--     fst <$> liftIO (evalTC g ir pmState $ tcInit >> extractImportedModules unit)

-- runModuleTypecheck :: QualName -> Map.Map Name FilePath -> CompiledModules -> Command ClsGraph (Either Compilation.ModuleCompilationError (Imports, CompiledModules))
-- runModuleTypecheck moduleName sources cmpMods@(CompiledModules _ prims) = do
--     unit :: Expr Unit <- uses Graph.clsClass unsafeGeneralize
--     g <- get
--     AST ir pmState <- use Graph.clsAst
--     (res, newG) <- liftIO $ evalTC g ir pmState $ do
--         tcInit
--         impNamesWithBase <- Set.toList <$> extractImportedModules unit
--         result <- liftIO $ Compilation.requestModules sources impNamesWithBase cmpMods
--         case result of
--             Right (imports, newCmpMods) -> do
--                 let imps = unionsImports $ prims : Map.elems imports
--                 res <- ModuleTC.processModule imps (convert moduleName) (IR.unsafeGeneralize unit)
--                 return $ Right (res, newCmpMods)
--             Left err -> return $ Left err
--     return res
