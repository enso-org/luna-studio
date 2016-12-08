{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Empire.ASTOps.Remove where

import           Prologue                      hiding ((#))
import           Data.Construction             (destruct)
import           Data.Layer_OLD.Cover_OLD      (uncover, covered)
import           Data.List                     (nub)

import           Empire.ASTOp                  (ASTOp)
import           Empire.Data.AST               (NodeRef, Inputs)


import Luna.IR as IR (delete, readLayer, source)
import Luna.IR.Layer.Succs (Succs)
import Data.Set (size)

removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = delete ref

safeRemove :: ASTOp m => NodeRef -> m ()
safeRemove ref = do
    refCount <- getRefCount ref
    if refCount > 0
        then return ()
        else performSafeRemoval ref

getRefCount :: ASTOp m => NodeRef -> m Int
getRefCount ref = IR.readLayer @Succs ref >>= pure . size

performSafeRemoval :: ASTOp m => NodeRef -> m ()
performSafeRemoval ref = do
    node <- pure ref
    inputs <- IR.readLayer @Inputs node
    toRemove <- fmap nub $ mapM IR.source $ inputs
    removeNode ref
    mapM_ safeRemove toRemove
