{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.Data.Graph where

import           Empire.Data.BreadcrumbHierarchy   (LamItem)
import           Empire.Prelude

import           Control.Monad.State               (MonadState(..), StateT, evalStateT, lift)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Empire.Data.AST                   (NodeRef, SomeASTException)
import           Empire.Data.Layers                (attachEmpireLayers)
import           LunaStudio.Data.Node              (NodeId)

import           Control.Monad.Raise                    (MonadException(..))
import qualified Control.Monad.State.Dependent          as DepState
import qualified Luna.Builtin.Data.Function             as Function
import           Luna.IR                                (IR, IRBuilder, AnyExpr, evalIRBuilder', evalPassManager',
                                                         attachLayer, snapshot, runRegs, Cache)
import qualified Luna.IR                                as IR
import qualified OCI.Pass.Class                         as Pass
import qualified OCI.Pass.Manager                       as Pass (RefState)
import qualified OCI.Pass.Manager                       as PassManager (PassManager, State)
import           LunaStudio.Data.Node                   (NodeId)
import           LunaStudio.Data.NodeCache
import           LunaStudio.Data.NodeMeta               (NodeMeta)
import           Luna.Syntax.Text.Parser.Errors         (Invalids)
import qualified Luna.Syntax.Text.Parser.Marker         as Luna
import qualified Luna.Syntax.Text.Parser.Parser         as Parser
import qualified Luna.Syntax.Text.Parser.Parsing        as Parser ()
import qualified Luna.Syntax.Text.Parser.CodeSpan       as CodeSpan
import           Data.TypeDesc                          (getTypeDesc)
import           Data.Text.Position                     (Delta)

import           System.Log                             (Logger, DropLogger, dropLogs, MonadLogging)

import qualified OCI.IR.Repr.Vis            as Vis
import           Control.Monad              (void)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Maybe                 (isJust)
import           System.Environment         (lookupEnv)
import           Web.Browser                (openBrowser)
import           Luna.Pass.Data.ExprMapping


data Graph = Graph { _ast                   :: AST Graph
                   , _breadcrumbHierarchy   :: LamItem
                   , _codeMarkers           :: Map Luna.MarkerId NodeRef
                   , _globalMarkers         :: Map Luna.MarkerId NodeRef
                   , _graphCode             :: Text
                   , _parseError            :: Maybe SomeException
                   , _fileOffset            :: Delta
                   , _graphNodeCache        :: NodeCache
                   } deriving Show

data FunctionGraph = FunctionGraph { _funName    :: String
                                   , _funGraph   :: Graph
                                   , _funMarkers :: Map Luna.MarkerId NodeRef
                                   } deriving Show

data ClsGraph = ClsGraph { _clsAst         :: AST ClsGraph
                         , _clsClass       :: NodeRef
                         , _clsCodeMarkers :: Map Luna.MarkerId NodeRef
                         , _clsCode        :: Text
                         , _clsParseError  :: Maybe SomeException
                         , _clsFuns        :: Map NodeId FunctionGraph
                         , _clsNodeCache   :: NodeCache
                         } deriving Show

defaultClsGraph :: IO ClsGraph
defaultClsGraph = do
    (ast, cls) <- defaultClsAST
    return $ ClsGraph ast cls def def def def def

type PMState g = Pass.RefState (PassManager.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO))))))

data AST g = AST { _ir      :: IR
                 , _pmState :: PMState g
                 }

instance Show (AST g) where
   show _ = "AST"

instance MonadState s m => MonadState s (DepState.StateT b m) where
    get = lift   get
    put = lift . put

instance MonadState s m => MonadState s (PassManager.PassManager m) where
    get = lift   get
    put = lift . put

instance MonadState s m => MonadState s (Logger DropLogger m) where
    get = lift   get
    put = lift . put

instance Exception e => MonadException e IO where
    raise = throwM

unescapeUnaryMinus :: Vis.Node -> Vis.Node
unescapeUnaryMinus n = if n ^. Vis.name == "Var \"#uminus#\""
                       then n & Vis.name .~ "Var uminus"
                       else n

withVis :: MonadIO m => Vis.VisStateT m a -> m a
withVis m = do
    (p, vis) <- Vis.newRunDiffT m
    when (not . null $ vis ^. Vis.steps) $ do
        let unescaped = vis & Vis.nodes %~ Map.map unescapeUnaryMinus
            cfg = ByteString.unpack $ encode unescaped
        showVis <- liftIO $ lookupEnv "DEBUGVIS"
        if isJust showVis then void $ liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> cfg else return ()
    return p

data InitPass
type instance IR.Abstract   InitPass = InitPass
type instance Pass.Inputs     IR.Net   InitPass = '[IR.AnyExpr, IR.AnyExprLink]
type instance Pass.Inputs     IR.Layer InitPass = '[IR.AnyExpr IR.// IR.Model, IR.AnyExprLink IR.// IR.Model, IR.AnyExpr IR.// IR.Succs, IR.AnyExpr IR.// IR.Type]
type instance Pass.Inputs     IR.Attr  InitPass = '[]
type instance Pass.Inputs     IR.Event InitPass = '[]

type instance Pass.Outputs    IR.Net   InitPass = '[IR.AnyExpr, IR.AnyExprLink]
type instance Pass.Outputs    IR.Layer InitPass = '[IR.AnyExpr IR.// IR.Model, IR.AnyExprLink IR.// IR.Model, IR.AnyExpr IR.// IR.Succs, IR.AnyExpr IR.// IR.Type]
type instance Pass.Outputs    IR.Attr  InitPass = '[]
type instance Pass.Outputs    IR.Event InitPass = '[IR.New IR.// IR.AnyExpr, IR.New IR.// IR.AnyExprLink]

type instance Pass.Preserves        InitPass = '[]

defaultPMState :: IO (PMState a)
defaultPMState = mdo
    flip evalStateT undefined $ withVis $ dropLogs $ DepState.evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        CodeSpan.init
        attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
        attachEmpireLayers
        initExprMapping
        pass <- DepState.get @PassManager.State
        return pass

emptyClsAST :: IO (AST ClsGraph)
emptyClsAST = mdo
    let g = ClsGraph ast undefined def def def def (NodeCache def def def)
    ast <- flip evalStateT g $ withVis $ dropLogs $ DepState.evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        CodeSpan.init
        attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
        attachEmpireLayers
        initExprMapping
        st   <- snapshot
        pass <- DepState.get @PassManager.State
        return $ AST st pass
    return ast

defaultClsAST :: IO (AST ClsGraph, IR.SomeExpr)
defaultClsAST = mdo
    let g = ClsGraph ast undefined def def def def (NodeCache def def def)
    (ast, cls) <- flip evalStateT g $ withVis $ dropLogs $ DepState.evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        CodeSpan.init
        attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @AnyExpr)
        attachEmpireLayers
        initExprMapping
        cls <- Pass.eval' @InitPass $ do
            hub   <- IR.unresolvedImpHub' []
            cls   <- IR.clsASG' False (stringToName "A") [] [] []
            klass <- IR.unit' hub [] cls
            return klass
        st   <- snapshot
        pass <- DepState.get @PassManager.State
        return (AST st pass, cls)
    return (ast, cls)

makeLenses ''Graph
makeLenses ''FunctionGraph
makeLenses ''ClsGraph
makeLenses ''AST

class HasCode g where
    code :: Lens' g Text

instance HasCode Graph where
    code = graphCode

instance HasCode ClsGraph where
    code = clsCode

class HasNodeCache g where
    nodeCache :: Lens' g NodeCache

instance HasNodeCache Graph where
    nodeCache = graphNodeCache

instance HasNodeCache ClsGraph where
    nodeCache = clsNodeCache
