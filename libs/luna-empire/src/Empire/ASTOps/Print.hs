{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Empire.ASTOps.Print where

import           Control.Lens                   (non)
import           Control.Monad                  ((<=<))
import           Data.List                      (delete)
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           Empire.Prelude

import           Empire.ASTOp                   (ASTOp, Printer, GraphOp, match)
import qualified Empire.ASTOps.Read             as ASTRead
import           Empire.Data.AST                (EdgeRef, NodeRef)
import           Empire.Data.Graph              (Graph)
import qualified Luna.IR                        as IR
import qualified Luna.IR.Aliases                as Uni
-- import           Luna.IR.Term.Uni
import           LunaStudio.Data.TypeRep

import qualified Luna.Syntax.Prettyprint        as Prettyprint
import           Luna.Syntax.Text.Lexer.Grammar (isOperator)
-- import           Luna.Syntax.Text.Pretty.Pretty as CodeGen


getTypeRep :: NodeRef -> GraphOp TypeRep
getTypeRep tp = match tp $ \case
    -- Monadic s _   -> getTypeRep =<< source s
    Uni.ResolvedCons _ n _ args -> TCons (nameToString n) <$> (mapM (getTypeRep <=< source) =<< ptrListToList args)
    Cons   n args -> TCons (nameToString n) <$> (mapM (getTypeRep <=< source) =<< ptrListToList args)
    Lam    a out  -> TLam <$> (getTypeRep =<< source a) <*> (getTypeRep =<< source out)
    Acc    t n    -> TAcc <$> (ASTRead.getVarName =<< source n) <*> (getTypeRep =<< source t)
    Var    n      -> return $ TVar $ delete '#' $ nameToString n
    IRNumber{}    -> return $ TCons "Number" []
    _             -> return TStar

instance (MonadIO m, Printer Graph m) => Prettyprint.Compactible Prettyprint.CompactStyle m where
    shouldBeCompact a = ASTRead.isGraphNode a

printExpression :: NodeRef -> GraphOp String
printExpression n = convert <$> Prettyprint.run @Prettyprint.CompactStyle def n

printFullExpression :: NodeRef -> ASTOp g Text
printFullExpression n = Prettyprint.run @Prettyprint.Simple def n

printName :: NodeRef -> GraphOp String
printName node = convert <$> Prettyprint.run @Prettyprint.Simple def node

printNodeTarget :: NodeRef -> GraphOp String
printNodeTarget ref = match ref $ \case
    Unify _ r -> printExpression =<< source r
    _         -> printExpression ref

genOperatorName :: IR.Name -> Text
genOperatorName op = operatorNamesMap ^. at op . non "operator" where
    operatorNamesMap = Map.fromList [ ("+",        "sum")
                                    , ("*",        "product")
                                    , ("-",        "difference")
                                    , ("/",        "quotient")
                                    , ("#uminus#", "negation")
                                    ]

genNodeBaseName :: NodeRef -> GraphOp Text
genNodeBaseName ref = match ref $ \case
    App f _           -> recurOn $ generalize f
    Grouped g         -> recurOn $ generalize g
    -- LeftSection  op _ -> recurOn $ generalize op
    -- RightSection op _ -> recurOn $ generalize op
    Marked _ a        -> recurOn $ generalize a
    Lam{}             -> return "lambda"
    IRString{}        -> return "text"
    IRNumber{}        -> return "number"
    Tuple{}           -> return "tuple"
    List{}            -> return "list"
    Cons n _          -> return $ Text.toLower $ nameToText n
    Var n             -> return $ genOp n
    Acc _ n           -> recurOn $ generalize n
    _                 -> return $ "expr"
    where recurOn :: EdgeRef -> GraphOp Text
          recurOn a = genNodeBaseName =<< source a
          genOp   n = if isOperator n  || n == "#uminus#" then genOperatorName n else nameToText n
