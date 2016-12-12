{-# LANGUAGE TypeApplications #-}
module Empire.Data.Graph where

import           Data.Map.Lazy                     (Map)
import qualified Data.Map                          as Map (empty)
import qualified Data.Set                          as Set (empty)
import           Empire.API.Data.Node              (NodeId)
import           Empire.Data.BreadcrumbHierarchy   (BreadcrumbHierarchy, empty)
import           Prologue

import           Empire.Data.AST                   (AST, ASTState(..), NodeRef, Marker, Meta,
                                                    Inputs, TypeLayer, TCData, TCDataMock(..))
import Luna.IR (LayerData, Model,
                EXPR, LINK', attachLayer, registerElemLayer, runIRT, runRegs, snapshot)
import Luna.IR.Layer.Succs (Succs)
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
type instance LayerData InterpreterData t = InterpreterLayer

prepareASTOp :: AST
prepareASTOp = unsafePerformIO $ liftIO $ runIRT $ do
    runRegs
    registerElemLayer @EXPR @Meta   $ \_ _ -> return Nothing
    registerElemLayer @EXPR @Marker $ \_ _ -> return Nothing
    registerElemLayer @EXPR @Inputs $ \_ _ -> return []
    registerElemLayer @EXPR @InterpreterData $ \_ _ -> return (def::InterpreterLayer)
    registerElemLayer @EXPR @Succs $ \_ _ -> return Set.empty
    registerElemLayer @EXPR @TCData $ \_ _ -> return $ TCDataMock []
    attachLayer (typeRep' @Model) (typeRep' @EXPR)
    attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
    attachLayer (typeRep' @Meta)  (typeRep' @EXPR)
    attachLayer (typeRep' @Marker) (typeRep' @EXPR)
    attachLayer (typeRep' @Inputs) (typeRep' @EXPR)
    attachLayer (typeRep' @TypeLayer) (typeRep' @EXPR)
    attachLayer (typeRep' @InterpreterData) (typeRep' @EXPR)
    attachLayer (typeRep' @Succs) (typeRep' @EXPR)
    attachLayer (typeRep' @TCData) (typeRep' @EXPR)
    st <- snapshot
    return $ ASTState st
