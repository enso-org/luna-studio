{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}

module Empire.ASTOp where

import           Prologue                                hiding (Cons, Curry, Num)

import           Control.Monad                           (foldM)
import           Control.Monad.State                     (get, put)
import           Control.Monad.Except                    (throwError)
import           Empire.Data.AST                         (AST, ASTState(..), Marker, Meta, NodeRef,
                                                          Inputs, TypeLayer, TCData)
import           Empire.Empire                           (Command)
import qualified Luna.Pass    as Pass (SubPass, Inputs, Outputs, Preserves, eval')
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)

import Luna.IR (IRMonad, Accessibles, ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers, Model,
                IRT(..), runIRT, unsafeRelayout, generalize, lam,
                putIRState, snapshot)
import qualified Luna.IR.Function as IR (Arg, arg)
import Luna.IR.Layer.Succs (Succs)

type ASTOp m = (MonadThrow m, IRMonad m,
                Accessibles m ('[ExprNet, ExprLinkNet] <>
                    ExprLayers '[Model, Marker, Meta, Inputs, Succs, InterpreterData, TCData, TypeLayer] <>
                    ExprLinkLayers '[Model]))

data EmpirePass
type instance Pass.Inputs  EmpirePass   = '[ExprNet, ExprLinkNet] <>
    ExprLayers '[Model, Meta, Marker, Inputs, TypeLayer, Succs, InterpreterData, TypeLayer, TCData] <>
    ExprLinkLayers '[Model]
type instance Pass.Outputs EmpirePass   = '[ExprNet, ExprLinkNet] <>
    ExprLayers '[Model, Meta, Marker, Inputs, TypeLayer, Succs, InterpreterData, TypeLayer, TCData] <>
    ExprLinkLayers '[Model]
type instance Pass.Preserves EmpirePass = '[]

runGraph :: a
runGraph = $notImplemented

runBuilder :: a
runBuilder = $notImplemented

runASTOp :: Pass.SubPass EmpirePass (IRT IO) a -> Command AST a
runASTOp pass = do
    ASTState currentState <- get
    (a, st) <- liftIO $ runIRT $ do
        putIRState currentState
        a <- Pass.eval' pass
        st <- snapshot
        return (a, st)
    put $ ASTState st
    case a of
        Left err -> throwError $ "pass internal error: " ++ show err
        Right res -> return res

lams :: ASTOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout output) (unsafeRelayout <$> args)
    where
        f arg' lam' = lamAny (IR.arg arg') lam'

lamAny :: ASTOp m => IR.Arg NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap generalize $ lam a b
