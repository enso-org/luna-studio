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
                                                          InputsLayer, TypeLayer, TCData)
import           Empire.Empire                           (Command)
import qualified Luna.Pass    as Pass (SubPass, Inputs, Outputs, Preserves, Events, eval')
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)
import GHC.Prim (Any)

import Data.Event (Emitter, type (//))
import Luna.IR (IRMonad, Accessibles, ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers, Model,
                unsafeRelayout, generalize, lam, evalIRBuilder, evalPassManager,
                snapshot, IRBuilder, NEW, LINK', EXPR, putIR, thaw, Abstract)
import qualified Luna.Pass.Manager as Pass (PassManager, get)
import qualified Luna.IR.Function as IR (Arg, arg)
import Luna.IR.Layer.Succs (Succs)

type ASTOp m = (MonadThrow m, IRMonad m, Emitter m (NEW // LINK' EXPR), Emitter m (NEW // EXPR),
                Accessibles m ('[ExprNet, ExprLinkNet] <>
                    ExprLayers '[Model, Marker, Meta, InputsLayer, Succs, InterpreterData, TCData, TypeLayer] <>
                    ExprLinkLayers '[Model]))

data EmpirePass
type instance Abstract EmpirePass = EmpirePass
type instance Pass.Inputs  EmpirePass   = '[ExprNet, ExprLinkNet] <>
    ExprLayers '[Model, Meta, Marker, InputsLayer, TypeLayer, Succs, InterpreterData, TypeLayer, TCData] <>
    ExprLinkLayers '[Model]
type instance Pass.Outputs EmpirePass   = '[ExprNet, ExprLinkNet] <>
    ExprLayers '[Model, Meta, Marker, InputsLayer, TypeLayer, Succs, InterpreterData, TypeLayer, TCData] <>
    ExprLinkLayers '[Model]
type instance Pass.Events EmpirePass = '[NEW // LINK' EXPR, NEW // EXPR]
type instance Pass.Preserves EmpirePass = '[]

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder IO)) a -> Command AST a
runASTOp pass = do
    ASTState currentStateIR currentStatePass <- get
    (a, (st, pass)) <- liftIO $ flip evalIRBuilder currentStateIR $ flip evalPassManager currentStatePass $ do
        a <- Pass.eval' pass
        st <- snapshot
        pass <- Pass.get
        return (a, (st, pass))
    put $ ASTState st pass
    case a of
        Left err -> throwError $ "pass internal error: " ++ show err
        Right res -> return res

lams :: ASTOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout output) (unsafeRelayout <$> args)
    where
        f arg' lam' = lamAny (IR.arg arg') lam'

lamAny :: ASTOp m => IR.Arg NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap generalize $ lam a b
