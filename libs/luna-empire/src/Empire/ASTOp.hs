{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}

module Empire.ASTOp where

import           Empire.Prelude

import           Control.Monad                           (foldM)
import           Control.Monad.State                     (get, put)
import           Control.Monad.Except                    (throwError)
import           Empire.Data.AST                         (AST, ASTState(..), NodeRef)
import           Empire.Data.Layers                      (Marker, Meta,
                                                          InputsLayer, TypeLayer, TCData)
import           Empire.Empire                           (Command)
import qualified Luna.Pass    as Pass (SubPass, Inputs, Outputs, Preserves, Events, eval')
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)

import Data.Event (Emitter, type (//))
import Luna.IR (IRMonad, Accessibles, ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers, Model,
                unsafeRelayout, generalize, lam, evalIRBuilder, evalPassManager,
                snapshot, IRBuilder, NEW, DELETE, LINK', EXPR, Abstract)
import qualified Luna.Pass.Manager as Pass (PassManager, get)
import qualified Luna.IR.Function as IR (Arg, arg)
import System.Log (Logger, DropLogger, dropLogs)
import Luna.IR.Layer.Succs (Succs)

type ASTOp m = (MonadThrow m, IRMonad m, Emitter m (NEW // LINK' EXPR), Emitter m (NEW // EXPR),
                Emitter m (DELETE // LINK' EXPR), Emitter m (DELETE // EXPR),
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
type instance Pass.Events EmpirePass = '[NEW // LINK' EXPR, NEW // EXPR, DELETE // LINK' EXPR, DELETE // EXPR]
type instance Pass.Preserves EmpirePass = '[]

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Logger DropLogger IO))) a -> Command AST a
runASTOp pass = do
    ASTState currentStateIR currentStatePass <- get
    (a, (st, passSt)) <- liftIO $ dropLogs $ flip evalIRBuilder currentStateIR $ flip evalPassManager currentStatePass $ do
        a <- Pass.eval' pass
        st <- snapshot
        passSt <- Pass.get
        return (a, (st, passSt))
    put $ ASTState st passSt
    case a of
        Left err -> throwError $ "pass internal error: " ++ show err
        Right res -> return res

lams :: ASTOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout seed) (unsafeRelayout <$> rest)
    where
        f arg' lam' = lamAny (IR.arg arg') lam'
        (seed : rest) = args ++ [output]

lamAny :: ASTOp m => IR.Arg NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap generalize $ lam a b
