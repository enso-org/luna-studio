module Empire.Commands.GraphUtils (
    getASTPointer
  , getASTTarget
  , getASTVar
  , rewireNode
  ) where

import           Empire.Prelude ()

import           Empire.ASTOps.Read      (getASTPointer, getASTTarget, getASTVar)
import           Empire.ASTOps.Modify    (rewireNode)
