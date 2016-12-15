{-# LANGUAGE TypeApplications #-}
module Empire.Data.Graph where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import qualified Data.Set                          as Set (empty)
import           Data.Typeable                     (typeRep)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy, empty)
import           Empire.Prelude

import           Empire.Data.AST                   (AST, ASTState(..), NodeRef, Marker, Meta,
                                                    InputsLayer, TypeLayer, TCData, TCDataMock(..),
                                                    registerEmpireLayers)
import Luna.IR (LayerData, Model, evalIRBuilder', evalPassManager', WorkingElem, writeLayer, ElemScope,
                EXPR, LINK', attachLayer, registerLayer, runRegs, snapshot, readAttr, IRMonad,
                MonadPassManager, EXPRESSION)
import Luna.IR.Layer.Succs (Succs)
import qualified Luna.Pass as Pass (Pass, DynPass, compile)
import qualified Luna.Pass.Manager as Pass (PassManager, get)
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData, InterpreterLayer)

import System.IO.Unsafe (unsafePerformIO)


data Graph = Graph { _ast                   :: AST
                   , _nodeMapping           :: Map NodeId NodeIDTarget
                   , _breadcrumbHierarchy   :: BreadcrumbHierarchy
                   , _breadcrumbPortMapping :: Map NodeId (NodeId, NodeId)
                   , _lastNameId            :: Integer
                   , _insideNode            :: Maybe NodeId
                   } deriving Show

data NodeIDTarget = MatchNode     NodeRef
                  | AnonymousNode NodeRef
    deriving Show

getAnyRef :: NodeIDTarget -> NodeRef
getAnyRef (MatchNode ref)     = ref
getAnyRef (AnonymousNode ref) = ref

makeLenses ''Graph

instance Default Graph where
    def = Graph defaultAST def empty Map.empty 0 Nothing

defaultAST :: AST
defaultAST = prepareASTOp

{-# NOINLINE prepareASTOp #-}
prepareASTOp :: AST
prepareASTOp = unsafePerformIO $ liftIO $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    registerEmpireLayers
    attachLayer 10 (typeRep' @Meta)  (typeRep' @EXPR)
    attachLayer 10 (typeRep' @Marker) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @InputsLayer) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @InterpreterData) (typeRep' @EXPR)
    attachLayer 10 (typeRep' @TCData) (typeRep' @EXPR)
    st <- snapshot
    pass <- Pass.get
    return $ ASTState st pass
