{-# LANGUAGE LambdaCase #-}

module Empire.ASTOps.Remove (
    removeArg
  , removeSubtree
  ) where

import           Control.Monad             (foldM)
import           Empire.Prelude

import           Empire.ASTOp              (GraphOp, match)
import           Empire.ASTOps.Deconstruct (deconstructApp)
import           Empire.ASTOps.Read        (isBlank)
import           Empire.Data.AST           (NodeRef)

import qualified Luna.IR                   as IR

removeSubtree :: NodeRef -> GraphOp ()
removeSubtree ref = deleteSubtree ref

-- | Creates new App node with Blank inserted at specified position
removeArg :: NodeRef -> Int -> GraphOp NodeRef
removeArg expr i = do
    (fun, args) <- deconstructApp expr
    b <- generalize <$> IR.blank
    let args' = args & ix i .~ b
    a <- apps fun args'
    removeTrailingBlanks a

apps :: Expr f -> [NodeRef] -> GraphOp NodeRef
apps fun exprs = coerce <$> foldM appAny (coerce fun) (coerce <$> exprs)

appAny :: NodeRef -> NodeRef -> GraphOp NodeRef
appAny = fmap generalize .: IR.app

removeTrailingBlanks :: NodeRef -> GraphOp NodeRef
removeTrailingBlanks expr = match expr $ \case
    App a c -> do
        argBlank <- isBlank =<< source c
        if argBlank then removeTrailingBlanks =<< source a
                    else return expr
    _ -> return expr

