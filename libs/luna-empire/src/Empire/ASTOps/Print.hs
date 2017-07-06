{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOps.Print where

import           Control.Monad                  (forM, (<=<))
import           Data.Char                      (isAlpha)
import           Data.List                      (delete, dropWhileEnd)
import           Empire.Prelude                 hiding (TypeRep)

import           Empire.ASTOp              (ASTOpReq, ASTOp, GraphOp, match)
import           Empire.Data.AST           (NodeRef)
import           Empire.Data.Graph         (Graph)
import qualified Empire.ASTOps.Read        as ASTRead
import qualified Empire.ASTOps.Deconstruct as ASTDeconstruct
import           LunaStudio.Data.Node      (NodeId)
import           LunaStudio.Data.TypeRep
import qualified Luna.IR                        as IR
import           Luna.IR.Term.Uni

import           Luna.Syntax.Text.Pretty.Pretty as CodeGen

getTypeRep :: GraphOp m => NodeRef -> m TypeRep
getTypeRep tp = match tp $ \case
    Monadic s _   -> getTypeRep =<< IR.source s
    Cons   n args -> TCons (nameToString n) <$> mapM (getTypeRep <=< IR.source) args
    Lam    a out  -> TLam <$> (getTypeRep =<< IR.source a) <*> (getTypeRep =<< IR.source out)
    Acc    t n    -> TAcc (nameToString n) <$> (getTypeRep =<< IR.source t)
    Var    n      -> return $ TVar $ delete '#' $ nameToString n
    Number _      -> return $ TCons "Number" []
    _             -> return TStar

instance ASTOpReq Graph m => Compactible t CompactStyle m where
    shouldBeCompact _ r = ASTRead.isGraphNode r

printExpression :: GraphOp m => NodeRef -> m String
printExpression = fmap convert . CodeGen.subpass CompactStyle . IR.unsafeGeneralize

printFullExpression :: GraphOp m => NodeRef -> m Text
printFullExpression = CodeGen.subpass SimpleStyle . IR.unsafeGeneralize

printName :: GraphOp m => NodeRef -> m String
printName = fmap convert . CodeGen.subpass SimpleStyle . IR.unsafeGeneralize

printNodeTarget :: GraphOp m => NodeRef -> m String
printNodeTarget ref = match ref $ \case
    Unify _ r -> printExpression =<< IR.source r
    _         -> printExpression ref
