module Empire.ASTOps.Print where

import           Prologue                 hiding (TypeRep)
import           Data.List                (dropWhileEnd, delete)
import qualified Data.Text.Lazy           as Text
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Char                (isAlpha)
import           Text.Printf              (printf)

import           Empire.ASTOp             (ASTOp)
import           Empire.Data.AST          (NodeRef)
import qualified Empire.ASTOps.Builder    as ASTBuilder
import           Empire.API.Data.TypeRep  (TypeRep (..))
import           Luna.IR.Expr.Term.Uni
import qualified Luna.IR as IR


getTypeRep :: ASTOp m => NodeRef -> m TypeRep
getTypeRep tp = match tp $ \case
      Cons (fromString -> s) as -> do
            args <- ASTBuilder.unpackArguments as
            argReps <- mapM getTypeRep args
            return $ TCons s argReps
        Lam as out -> do
            args   <- ASTBuilder.unpackArguments as
            argReps <- mapM getTypeRep args
            outRep <- getTypeRep =<< IR.source out
            return $ TLam argReps outRep
        Acc (fromString -> n) t -> do
            rep <- IR.source t >>= getTypeRep
            return $ TAcc n rep
        Var (fromString -> n) -> return $ TVar $ delete '#' n
        Star -> return TStar
        _ -> return TBlank

parenIf :: Bool -> String -> String
parenIf False s = s
parenIf True  s = "(" ++ s ++ ")"

printFunctionArguments :: ASTOp m => NodeRef -> m [String]
printFunctionArguments lam = do
    l <- Builder.read lam
    caseTest (uncover l) $ do
        Lam args _) -> do
            args' <- ASTBuilder.unpackArguments args
            mapM printExpression args'

printReturnValue :: ASTOp m => NodeRef -> m String
printReturnValue lam = do
    l <- Builder.read lam
    caseTest (uncover l) $ do
        Lam _ out) -> do
            out' <- Builder.follow source out
            printExpression out'

printFunctionHeader :: ASTOp m => NodeRef -> m String
printFunctionHeader function = do
    f <- Builder.read function
    caseTest (uncover f) $ do
        Match l r) -> do
            name <- Builder.follow source l >>= printExpression
            args <- Builder.follow source r >>= printFunctionArguments
            return $ "def " ++ name ++ " " ++ unwords args ++ ":"

printExpression' :: ASTOp m => Bool -> Bool -> NodeRef -> m String
printExpression' suppressNodes paren nodeRef = do
    let recur = printExpression' suppressNodes
    node <- Builder.read nodeRef
    let displayFun funExpr args = do
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- mapM (recur True) unpackedArgs
            if all (not . isAlpha) funExpr && length argsRep == 2
                then return $ parenIf paren $ head argsRep ++ " " ++ funExpr ++ " " ++ (argsRep !! 1)
                else do
                    let dropTailBlanks = dropWhileEnd (== "_") argsRep
                    let shouldParen = paren && not (null args)
                    case argsRep of
                        a : as -> return $ parenIf shouldParen $ funExpr ++ " " ++ unwords dropTailBlanks
                        _ -> return funExpr

    caseTest (uncover node) $ do
        Lam as o) -> do
            args    <- ASTBuilder.unpackArguments as
            argReps <- mapM (printExpression' False False) args
            out     <- Builder.follow source o
            sugared <- and <$> mapM ASTBuilder.isBlank args
            repr    <- printExpression' False sugared out
            let bindsRep = if sugared then "" else "-> " ++ unwords (('$' :) <$> argReps) ++ " "
            return $ parenIf (not sugared && paren) $ bindsRep ++ repr
        Match l r) -> do
            leftRep  <- Builder.follow source l >>= recur paren
            rightRep <- Builder.follow source r >>= recur paren
            return $ leftRep ++ " = " ++ rightRep
        Var n) -> do
            isNode <- ASTBuilder.isGraphNode nodeRef
            return $ if isNode && suppressNodes then "_" else unwrap n
        Acc n t) -> do
            targetRef <- Builder.follow source t
            target    <- Builder.read targetRef
            caseTest (uncover target) $ do
                Blank -> return $ "_." <> unwrap n
                _ -> do
                    targetRep <- recur True targetRef
                    return $ if targetRep == "_" then unwrap n else targetRep <> "." <> unwrap n
        App f args) -> do
            funExpr <- Builder.follow source f >>= recur True
            displayFun funExpr args
        Curry f args) -> do
            funExpr <- Builder.follow source f >>= recur True
            displayFun ("@" <> funExpr) args
        Blank -> return "_"
        Lit.Number _ s) -> return $ case s of
            Lit.Rational r -> show r
            Lit.Integer  i -> show i
            Lit.Double   d -> printf "%f" d
        Lit.String s) -> return $ show s
        Cons (Lit.String n) _) -> return n
        _ -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' False False

printNodeExpression :: ASTOp m => NodeRef -> m String
printNodeExpression = printExpression' True False
