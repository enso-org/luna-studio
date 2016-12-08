module Empire.ASTOps.Print where

import           Prologue                 hiding (TypeRep)
import           Data.List                (dropWhileEnd, delete)
import           Old.Data.Record              (ANY (..), caseTest, of')
import qualified Data.Text.Lazy           as Text
import           Data.Layer_OLD.Cover_OLD (uncover, covered)
import           Old.Data.Direction           (source)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Char                (isAlpha)
import           Text.Printf              (printf)

import           Empire.ASTOp             (ASTOp)
import           Empire.Data.AST          (NodeRef)
import qualified Empire.ASTOps.Builder    as ASTBuilder
import           Empire.API.Data.TypeRep  (TypeRep (..))

import           Old.Luna.Syntax.Term.Class   (Acc (..), App (..), Blank (..), Match (..), Var (..), Cons (..), Curry (..), Lam (..))
import qualified Old.Luna.Syntax.Term.Expr.Lit     as Lit

import qualified Old.Luna.Syntax.Model.Network.Builder as Builder

getTypeRep :: ASTOp m => NodeRef -> m TypeRep
getTypeRep tp = do
    tpNode <- Builder.read tp
    caseTest (uncover tpNode) $ do
        of' $ \(Cons (Lit.String s) as) -> do
            args <- ASTBuilder.unpackArguments as
            argReps <- mapM getTypeRep args
            return $ TCons s argReps
        of' $ \(Lam as out) -> do
            args   <- ASTBuilder.unpackArguments as
            argReps <- mapM getTypeRep args
            outRep <- getTypeRep =<< Builder.follow source out
            return $ TLam argReps outRep
        of' $ \(Acc (Lit.String n) t) -> do
            rep <- Builder.follow source t >>= getTypeRep
            return $ TAcc n rep
        of' $ \(Var (Lit.String n)) -> return $ TVar $ delete '#' n
        of' $ \Lit.Star -> return TStar
        of' $ \ANY -> return TBlank

parenIf :: Bool -> String -> String
parenIf False s = s
parenIf True  s = "(" ++ s ++ ")"

printFunctionArguments :: ASTOp m => NodeRef -> m [String]
printFunctionArguments lam = do
    l <- Builder.read lam
    caseTest (uncover l) $ do
        of' $ \(Lam args _) -> do
            args' <- ASTBuilder.unpackArguments args
            mapM printExpression args'

printReturnValue :: ASTOp m => NodeRef -> m String
printReturnValue lam = do
    l <- Builder.read lam
    caseTest (uncover l) $ do
        of' $ \(Lam _ out) -> do
            out' <- Builder.follow source out
            printExpression out'

printFunctionHeader :: ASTOp m => NodeRef -> m String
printFunctionHeader function = do
    f <- Builder.read function
    caseTest (uncover f) $ do
        of' $ \(Match l r) -> do
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
        of' $ \(Lam as o) -> do
            args    <- ASTBuilder.unpackArguments as
            argReps <- mapM (printExpression' False False) args
            out     <- Builder.follow source o
            sugared <- and <$> mapM ASTBuilder.isBlank args
            repr    <- printExpression' False sugared out
            let bindsRep = if sugared then "" else "-> " ++ unwords (('$' :) <$> argReps) ++ " "
            return $ parenIf (not sugared && paren) $ bindsRep ++ repr
        of' $ \(Match l r) -> do
            leftRep  <- Builder.follow source l >>= recur paren
            rightRep <- Builder.follow source r >>= recur paren
            return $ leftRep ++ " = " ++ rightRep
        of' $ \(Var n) -> do
            isNode <- ASTBuilder.isGraphNode nodeRef
            return $ if isNode && suppressNodes then "_" else unwrap n
        of' $ \(Acc n t) -> do
            targetRef <- Builder.follow source t
            target    <- Builder.read targetRef
            caseTest (uncover target) $ do
                of' $ \Blank -> return $ "_." <> unwrap n
                of' $ \ANY -> do
                    targetRep <- recur True targetRef
                    return $ if targetRep == "_" then unwrap n else targetRep <> "." <> unwrap n
        of' $ \(App f args) -> do
            funExpr <- Builder.follow source f >>= recur True
            displayFun funExpr args
        of' $ \(Curry f args) -> do
            funExpr <- Builder.follow source f >>= recur True
            displayFun ("@" <> funExpr) args
        of' $ \Blank -> return "_"
        of' $ \(Lit.Number _ s) -> return $ case s of
            Lit.Rational r -> show r
            Lit.Integer  i -> show i
            Lit.Double   d -> printf "%f" d
        of' $ \(Lit.String s) -> return $ show s
        of' $ \(Cons (Lit.String n) _) -> return n
        of' $ \ANY -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' False False

printNodeExpression :: ASTOp m => NodeRef -> m String
printNodeExpression = printExpression' True False
