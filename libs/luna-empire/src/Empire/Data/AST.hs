module Empire.Data.AST where

import           Empire.Prelude

import           Control.Monad.State (StateT)
import           Empire.Data.Layers  (attachEmpireLayers, registerEmpireLayers)

import           Luna.IR            (AnyExpr, AnyExprLink, IR, IRBuilder,
                                     evalIRBuilder', evalPassManager', snapshot,
                                     runRegs)
import qualified Luna.Pass.Manager  as Pass (State)
import qualified Luna.Pass.Manager  as PassManager (get)

import           System.Log         (Logger, DropLogger, dropLogs)


type NodeRef       = AnyExpr
type EdgeRef       = AnyExprLink
