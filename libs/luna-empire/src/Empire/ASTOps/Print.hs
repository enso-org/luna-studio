{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Empire.ASTOps.Print (
    getTypeRep
  , printExpression
  , printNodeExpression
  , printFunction
  ) where

import           Empire.Prelude
import           Control.Monad            ((<=<))
import           Data.List                (dropWhileEnd, delete)
import           Data.Char                (isAlpha)

import           Empire.ASTOp              (ASTOp)
import           Empire.Data.AST           (NodeRef)
import qualified Empire.ASTOps.Builder     as ASTBuilder
import qualified Empire.ASTOps.Read        as ASTRead
import qualified Empire.ASTOps.Deconstruct as ASTDeconstruct
import           Empire.API.Data.Node      (NodeId)
import           Empire.API.Data.TypeRep   (TypeRep (..))
import           Luna.IR.Expr.Term.Uni
import           Luna.IR.Function.Argument (Arg(..))
import           Luna.IR (match)
import qualified Luna.IR as IR


getTypeRep :: ASTOp m => NodeRef -> m TypeRep
getTypeRep tp = match tp $ \case
    Cons n args -> do
        name    <- ASTRead.getName n
        argReps <- mapM (getTypeRep <=< IR.source) $ map (\(Arg _ n) -> n) args
        return $ TCons name argReps
    Lam _as out -> do
        args   <- ASTDeconstruct.extractArguments tp
        argReps <- mapM getTypeRep args
        outRep <- getTypeRep =<< IR.source out
        return $ TLam argReps outRep
    Acc n t -> do
        name <- ASTRead.getName n
        rep <- IR.source t >>= getTypeRep
        return $ TAcc name rep
    Var n -> do
        name <- ASTRead.getName n
        return $ TVar $ delete '#' name
    Star -> return TStar
    _ -> return TBlank

parenIf :: Bool -> String -> String
parenIf False s = s
parenIf True  s = "(" ++ s ++ ")"

printFunction :: ASTOp m => NodeId -> m (String, String)
printFunction nodeId = do
    ptr <- ASTRead.getASTPointer nodeId
    header <- printFunctionHeader ptr
    lam <- ASTRead.getASTTarget nodeId
    ret <- printReturnValue lam
    return (header, ret)

printFunctionArguments :: ASTOp m => NodeRef -> m [String]
printFunctionArguments lam = match lam $ \case
    Lam _args _ -> do
        args' <- ASTDeconstruct.extractArguments lam
        mapM printExpression args'

printReturnValue :: ASTOp m => NodeRef -> m String
printReturnValue lam = match lam $ \case
    Lam _ out -> do
        out' <- IR.source out
        printExpression out'

printFunctionHeader :: ASTOp m => NodeRef -> m String
printFunctionHeader function = match function $ \case
    Unify l r -> do
        name <- IR.source l >>= printExpression
        args <- IR.source r >>= printFunctionArguments
        return $ "def " ++ name ++ " " ++ unwords args ++ ":"

printExpression' :: ASTOp m => Bool -> Bool -> NodeRef -> m String
printExpression' suppressNodes paren node = do
    let recur = printExpression' suppressNodes
    let displayFun funExpr node' = do
            unpackedArgs <- ASTDeconstruct.extractArguments node'
            argsRep <- mapM (recur True) unpackedArgs
            if all (not . isAlpha) funExpr && length argsRep == 2
                then return $ parenIf paren $ head argsRep ++ " " ++ funExpr ++ " " ++ (argsRep !! 1)
                else do
                    let dropTailBlanks = dropWhileEnd (== "_") argsRep
                    let shouldParen = paren && not (null unpackedArgs)
                    case argsRep of
                        a : as -> return $ parenIf shouldParen $ funExpr ++ " " ++ unwords dropTailBlanks
                        _ -> return funExpr

    match node $ \case
        Lam _as o -> do
            args    <- ASTDeconstruct.extractArguments node
            argReps <- mapM (printExpression' False False) args
            out     <- IR.source o
            sugared <- and <$> mapM ASTRead.isBlank args
            repr    <- printExpression' False sugared out
            let bindsRep = if sugared then "" else "-> " ++ unwords (('$' :) <$> argReps) ++ " "
            return $ parenIf (not sugared && paren) $ bindsRep ++ repr
        Unify l r -> do
            leftRep  <- IR.source l >>= recur paren
            rightRep <- IR.source r >>= recur paren
            return $ leftRep ++ " = " ++ rightRep
        Var n -> do
            name <- ASTRead.getName n
            isNode <- ASTRead.isGraphNode node
            return $ if isNode && suppressNodes then "_" else name
        Acc n t -> do
            name <- ASTRead.getName n
            target <- IR.source t
            match target $ \case
                Blank -> return $ "_." <> name
                _ -> do
                    targetRep <- recur True target
                    return $ if targetRep == "_" then name else targetRep <> "." <> name
        App f _args -> do
            funExpr <- IR.source f >>= recur True
            displayFun funExpr node
        Blank -> return "_"
        IR.Rational r -> pure $ show r
        IR.Integer  i -> pure $ show i
        IR.String s -> return $ show s
        Cons n _ -> ASTRead.getName n
        _ -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' False False

printNodeExpression :: ASTOp m => NodeRef -> m String
printNodeExpression = printExpression' True False
