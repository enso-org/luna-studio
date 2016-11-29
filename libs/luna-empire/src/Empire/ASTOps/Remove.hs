{-# LANGUAGE FlexibleContexts #-}

module Empire.ASTOps.Remove where

import           Prologue                      hiding ((#))
import           Data.Construction             (destruct)
import           Data.Container                (size)
import           Old.Data.Prop                 ((#))
import           Data.Layer_OLD.Cover_OLD      (uncover, covered)
import           Data.Graph                    (Inputs (..), Succs (..))
import           Data.Direction                (source)
import           Data.List                     (nub)

import           Empire.ASTOp                  (ASTOp)
import           Empire.Data.AST               (NodeRef)

import qualified Old.Luna.Syntax.Model.Network.Builder as Builder
import           Old.Luna.Syntax.Model.Network.Builder (Type (..))

removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = do
    void $ destruct ref

safeRemove :: ASTOp m => NodeRef -> m ()
safeRemove ref = do
    refCount <- getRefCount ref
    if refCount > 0
        then return ()
        else performSafeRemoval ref

getRefCount :: ASTOp m => NodeRef -> m Int
getRefCount ref = (size . (# Succs)) <$> Builder.read ref

performSafeRemoval :: ASTOp m => NodeRef -> m ()
performSafeRemoval ref = do
    node <- Builder.read ref
    toRemove <- fmap nub $ mapM (Builder.follow source) $ uncover node # Inputs
    removeNode ref
    mapM_ safeRemove toRemove

