{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOp (
    ASTOp
  , runASTOp
  , lams
  ) where

import           Empire.Prelude

import           Control.Monad        (foldM)
import           Control.Monad.State  (get, put)
import           Control.Monad.Except (throwError)
import           Empire.Data.AST      (AST, ASTState(..), NodeRef)
import           Empire.Data.Graph    (Graph)
import qualified Empire.Data.Graph    as Graph (ast)
import           Empire.Data.Layers   (Marker, Meta,
                                      InputsLayer, TypeLayer, TCData)
import           Empire.Empire        (Command)

import           Data.Event           (Emitters, type (//))
import           Luna.IR              (Abstract, Accessibles, IRBuilder, IRMonad,
                                       ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers,
                                       Model, NEW, DELETE, LINK', EXPR,
                                       unsafeRelayout, generalize, lam, evalIRBuilder,
                                       evalPassManager, snapshot)
import qualified Luna.IR.Function     as IR (Arg, arg)
import           Luna.IR.Layer.Succs  (Succs)
import           Luna.Pass            (Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass            as Pass (SubPass, eval')
import qualified Luna.Pass.Manager    as Pass (PassManager, get)
import           Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)

import           System.Log (Logger, DropLogger, dropLogs)


type ASTOp m = (MonadThrow m, IRMonad m,
                Emitters m EmpireEmitters,
                Accessibles m EmpireAccessibles)

type EmpireAccessibles = '[ExprNet, ExprLinkNet] <>
          ExprLayers     '[Model,
                           Marker,
                           Meta,
                           InputsLayer,
                           Succs,
                           InterpreterData,
                           TCData,
                           TypeLayer] <>
          ExprLinkLayers '[Model]

type EmpireEmitters = '[NEW // LINK' EXPR,
                        NEW // EXPR,
                        DELETE // LINK' EXPR,
                        DELETE // EXPR]

data EmpirePass
type instance Abstract  EmpirePass = EmpirePass
type instance Inputs    EmpirePass = EmpireAccessibles
type instance Outputs   EmpirePass = EmpireAccessibles
type instance Events    EmpirePass = EmpireEmitters
type instance Preserves EmpirePass = '[]

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Logger DropLogger IO))) a
         -> Command Graph a
runASTOp pass = zoom Graph.ast $ do
    ASTState currentStateIR currentStatePass <- get
    let evalIR = dropLogs . flip evalIRBuilder currentStateIR . flip evalPassManager currentStatePass
    (a, (st, passSt)) <- liftIO $ evalIR $ do
        a      <- Pass.eval' pass
        st     <- snapshot
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
