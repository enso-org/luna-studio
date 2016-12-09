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

import           Control.Monad.Error                     (throwError)
import           Data.Construction                       (Destructor, Unregister)
import           Empire.Data.AST                         (AST, EdgeRef, NodeRef, Marker, Meta,
                                                          Inputs, TypeLayer, TCData)
import           Empire.Empire                           (Command, Error, empire)
import           Luna.IR.Layer.Loc                      (LocationT, MonadLocation)
import qualified Luna.IR.Layer.Loc                      as Location
import qualified Luna.Pass    as Pass (SubPass, Inputs, Outputs, Preserves, eval')
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)
import           Type.Inference

import Luna.IR (IRMonad, Accessibles, ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers, Model,
                IRT(..), EXPR, LINK', attachLayer, registerElemLayer, runRegs, runIRT, blank, generalize)
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

deriving instance MonadThrow a => MonadThrow (IRT a)

runASTOp :: Pass.SubPass EmpirePass (IRT IO) a -> Command AST a
runASTOp pass = do
    a <- liftIO $ runIRT $ do
        runRegs
        registerElemLayer @EXPR @Meta   $ \_ _ -> return Nothing
        registerElemLayer @EXPR @Marker $ \_ _ -> return Nothing
        registerElemLayer @EXPR @Inputs $ \_ _ -> return []
        registerElemLayer @EXPR @TypeLayer $ \_ _ -> $notImplemented
        registerElemLayer @EXPR @InterpreterData $ \_ _ -> $notImplemented
        attachLayer (typeRep' @Model) (typeRep' @EXPR)
        attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
        attachLayer (typeRep' @Meta)  (typeRep' @EXPR)
        attachLayer (typeRep' @Marker) (typeRep' @EXPR)
        attachLayer (typeRep' @Inputs) (typeRep' @EXPR)
        attachLayer (typeRep' @TypeLayer) (typeRep' @EXPR)
        attachLayer (typeRep' @InterpreterData) (typeRep' @EXPR)
        Pass.eval' pass
    case a of
        Left _ -> throwError "pass internal error"
        Right a -> return a
