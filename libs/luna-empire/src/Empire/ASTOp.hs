{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOp (
    ASTOp
  , runASTOp
  ) where

import           Empire.Prelude

import           Control.Monad.State  (StateT, runStateT, get, put)
import           Control.Monad.Except (throwError)
import           Empire.Data.Graph    (ASTState(..), Graph)
import qualified Empire.Data.Graph    as Graph (ast)
import           Empire.Data.Layers   (Marker, Meta,
                                      InputsLayer, TypeLayer, TCData)
import           Empire.Empire        (Command)

import           Control.Monad.Raise  (tryAll)
import           Data.Event           (Emitters, type (//))
import           Data.Graph.Class     (MonadRefLookup(..), Net)
import           Luna.IR              hiding (get, put)
import           Luna.IR.Layer.Succs  (Succs)
import           Luna.Pass            (Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass            as Pass (SubPass, eval')
import qualified Luna.Pass.Manager    as Pass (PassManager, RefCache, get)
import           Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData)

import           System.Log (Logger, DropLogger, dropLogs)


type ASTOp m = (MonadThrow m, MonadPassManager m,
                MonadIO m,
                MonadState Graph m,
                Emitters EmpireEmitters m,
                Editors Net '[AnyExpr, AnyExprLink] m,
                Editors Layer EmpireLayers m)


type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
                      AnyExpr // Marker,
                      AnyExpr // Meta,
                      AnyExpr // InputsLayer,
                      AnyExpr // Succs,
                      AnyExpr // InterpreterData,
                      AnyExpr // TCData,
                      AnyExpr // TypeLayer,
                      AnyExpr // UID, AnyExprLink // UID]

type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
                        Import // AnyExpr, Import // AnyExprLink,
                        Delete // AnyExpr, Delete // AnyExprLink]

data EmpirePass
type instance Abstract   EmpirePass = EmpirePass
type instance Inputs     Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Inputs     Layer EmpirePass = EmpireLayers
type instance Inputs     Attr  EmpirePass = '[]
type instance Inputs     Event EmpirePass = '[]

type instance Outputs    Net   EmpirePass = '[AnyExpr, AnyExprLink]
type instance Outputs    Layer EmpirePass = EmpireLayers
type instance Outputs    Attr  EmpirePass = '[]
type instance Outputs    Event EmpirePass = EmpireEmitters

type instance Preserves        EmpirePass = '[]

instance MonadPassManager m => MonadRefLookup Net (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Event (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Layer (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

instance MonadPassManager m => MonadRefLookup Attr (Pass.SubPass pass m) where
    uncheckedLookupRef = lift . uncheckedLookupRef

runASTOp :: Pass.SubPass EmpirePass (Pass.PassManager (IRBuilder (Pass.RefCache (Logger DropLogger (StateT Graph IO))))) a
         -> Command Graph a
runASTOp pass = do
    g <- get
    ASTState currentStateIR currentStatePass <- use Graph.ast
    let evalIR = flip runStateT g
               . dropLogs
               . runRefCache
               . flip evalIRBuilder currentStateIR
               . flip evalPassManager currentStatePass
    ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
        a      <- Pass.eval' pass
        st     <- snapshot
        passSt <- Pass.get
        return (a, (st, passSt))
    put $ newG & Graph.ast .~ ASTState st passSt
    return a
    -- case a of
    --     Left err -> throwError $ "pass internal error: " ++ show err
    --     Right res -> return _
