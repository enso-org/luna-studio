module Empire.Data.AST where

import           Empire.Prelude

import           Empire.Data.Layers (attachEmpireLayers, registerEmpireLayers)

import           Luna.IR            (AnyExpr, AnyExprLink, IR, IRBuilder,
                                     evalIRBuilder', evalPassManager', snapshot,
                                     runRegs)
import qualified Luna.Pass.Manager  as Pass (State)
import qualified Luna.Pass.Manager  as PassManager (get)

import           System.Log         (Logger, DropLogger, dropLogs)


type AST           = ASTState
type NodeRef       = AnyExpr
type EdgeRef       = AnyExprLink


data ASTState = ASTState IR (Pass.State (IRBuilder (Logger DropLogger IO)))

instance Show ASTState where
    show _ = "AST"

defaultAST :: IO AST
defaultAST = dropLogs $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    registerEmpireLayers
    attachEmpireLayers
    st <- snapshot
    pass <- PassManager.get
    return $ ASTState st pass
