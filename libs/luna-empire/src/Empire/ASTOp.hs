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
import           Data.Set                                as Set (empty)
import           Empire.Data.AST                         (AST, EdgeRef, NodeRef, Marker, Meta,
                                                          Inputs, TypeLayer, TCData, TCDataMock(..))
import           Empire.Empire                           (Command, Error, empire)
import           Luna.IR.Layer.Loc                      (LocationT, MonadLocation)
import qualified Luna.IR.Layer.Loc                      as Location
import qualified Luna.Pass    as Pass (SubPass, Inputs, Outputs, Preserves, eval')
import Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData, InterpreterLayer)
import           Type.Inference

import Luna.IR (IRMonad, Accessibles, ExprNet, ExprLinkNet, ExprLinkLayers, ExprLayers, LayerData, Model,
                IRT(..), EXPR, LINK', attachLayer, registerElemLayer, runRegs, runIRT, blank, generalize,
                layerReg4)
import Luna.IR.Layer.Succs (Succs)
import qualified Luna.IR.Internal.LayerStore as Store

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

runGraph :: a
runGraph = $notImplemented

runBuilder :: a
runBuilder = $notImplemented

type instance LayerData InterpreterData t = InterpreterLayer

runASTOp :: Pass.SubPass EmpirePass (IRT IO) a -> Command AST a
runASTOp pass = do
    a <- liftIO $ runIRT $ do
        runRegs
        registerElemLayer @EXPR @Meta   $ \_ _ -> return Nothing
        registerElemLayer @EXPR @Marker $ \_ _ -> return Nothing
        registerElemLayer @EXPR @Inputs $ \_ _ -> return []
        registerElemLayer @EXPR @InterpreterData $ \_ _ -> return (def::InterpreterLayer)
        registerElemLayer @EXPR @Succs $ \_ _ -> return Set.empty
        registerElemLayer @EXPR @TCData $ \_ _ -> return $ TCDataMock []
        layerReg4
        -- registerElemLayer @EXPR @Type . consTypeLayer =<< runInIR (Store.newSTRef Nothing)
        attachLayer (typeRep' @Model) (typeRep' @EXPR)
        attachLayer (typeRep' @Model) (typeRep' @(LINK' EXPR))
        attachLayer (typeRep' @Meta)  (typeRep' @EXPR)
        attachLayer (typeRep' @Marker) (typeRep' @EXPR)
        attachLayer (typeRep' @Inputs) (typeRep' @EXPR)
        attachLayer (typeRep' @TypeLayer) (typeRep' @EXPR)
        attachLayer (typeRep' @InterpreterData) (typeRep' @EXPR)
        attachLayer (typeRep' @Succs) (typeRep' @EXPR)
        attachLayer (typeRep' @TCData) (typeRep' @EXPR)
        Pass.eval' pass
    case a of
        Left err -> throwError $ "pass internal error: " ++ show err
        Right a -> return a
