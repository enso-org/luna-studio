{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Empire.ASTOps.Print where

import           Control.Monad                  (forM, (<=<))
import           Data.Char                      (isAlpha)
import           Data.List                      (delete, dropWhileEnd)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           Empire.Prelude                 hiding (TypeRep, List)

import           Empire.ASTOp                   (ASTOpReq, ASTOp, GraphOp, match)
import           Empire.Data.AST                (NodeRef)
import           Empire.Data.Graph              (Graph)
import qualified Empire.ASTOps.Read             as ASTRead
import qualified Empire.ASTOps.Deconstruct      as ASTDeconstruct
import           LunaStudio.Data.Node           (NodeId)
import qualified Luna.IR                        as IR
import           LunaStudio.Data.TypeRep
import           Luna.IR.Term.Uni

import           Luna.Syntax.Text.Lexer.Name    (isOperator)
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

genOperatorName :: IR.Name -> Text
genOperatorName op = operatorNamesMap ^. at op . non "operator" where
    operatorNamesMap = Map.fromList [ ("+",       "sum")
                                    , ("*",       "product")
                                    , ("-",       "difference")
                                    , ("/",       "quotient")
                                    , ("#minus#", "negation")
                                    ]

genNodeBaseName :: GraphOp m => NodeRef -> m Text
genNodeBaseName ref = match ref $ \case
    App f a           -> recurOn f
    Grouped g         -> recurOn g
    LeftSection  op _ -> recurOn op
    RightSection op _ -> recurOn op
    Marked _ a        -> recurOn a
    Lam{}             -> return "lambda"
    String{}          -> return "text"
    Number{}          -> return "number"
    Tuple{}           -> return "tuple"
    List{}            -> return "list"
    Cons n _          -> return $ Text.toLower $ convert n
    Var n             -> return $ genOp n
    Acc t n           -> return $ genOp n
    _                 -> return $ "expr"
    where recurOn a = genNodeBaseName =<< IR.source a
          genOp   n = if isOperator n then genOperatorName n else convert n
