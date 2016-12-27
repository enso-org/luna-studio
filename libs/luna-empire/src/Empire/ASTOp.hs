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
import           Control.Monad.State  (StateT, runStateT, get, put)
import           Control.Monad.Except (throwError)
import           Empire.Data.AST      (NodeRef)
import           Empire.Data.Graph    (AST, ASTState(..), Graph)
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
                MonadIO m,
                MonadState Graph m,
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

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Logger DropLogger (StateT Graph IO)))) a
         -> Command Graph a
runASTOp pass = do
    g <- get
    ASTState currentStateIR currentStatePass <- use Graph.ast
    let evalIR = flip runStateT g
               . dropLogs
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
        a      <- Pass.eval' pass
        st     <- snapshot
        passSt <- Pass.get
        return (a, (st, passSt))
    put $ newG & Graph.ast .~ ASTState st passSt
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
