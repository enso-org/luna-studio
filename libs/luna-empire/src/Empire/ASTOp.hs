{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOp where

import           Prologue                                hiding (Cons, Curry, Num)

import           Control.Monad.Error                     (ErrorT, MonadError, runErrorT)
import           Data.Construction                       (Destructor, Unregister)
import           Old.Data.Graph.Builder                      (MonadBuilder)
import           Old.Data.Graph.Builders                     (Connectible)
import           Old.Data.Graph.Model.Events                 (ELEMENT (..))
import           Empire.Data.AST                         (AST, EdgeRef, NodeRef)
import           Empire.Empire                           (Command, Error, empire)
import qualified Old.Luna.Syntax.Model.Network.Builder.Class as Builder
import           Old.Luna.Syntax.Model.Network.Builder.Term  (NetworkBuilderT, TermBuilder_OLD, runNetworkBuilderT)
import           Luna.IR.Layer.Loc                      (LocationT, MonadLocation)
import qualified Luna.IR.Layer.Loc                      as Location
import           Old.Luna.Syntax.Term.Class              (Acc, App, Blank, Cons, Curry, Lam, Match, Unify, Var)
import qualified Old.Luna.Syntax.Term.Expr.Lit           as Lit
import           Type.Inference

type ASTOp m = ( MonadIO m
               , MonadFix m
               , MonadError Error m
               , Destructor m NodeRef
               , Unregister m EdgeRef
               , MonadBuilder AST m
               , MonadLocation m
               , TermBuilder_OLD Blank      m NodeRef
               , TermBuilder_OLD Lit.Number m NodeRef
               , TermBuilder_OLD Lit.String m NodeRef
               , TermBuilder_OLD Acc        m NodeRef
               , TermBuilder_OLD App        m NodeRef
               , TermBuilder_OLD Match      m NodeRef
               , TermBuilder_OLD Var        m NodeRef
               , TermBuilder_OLD Unify      m NodeRef
               , TermBuilder_OLD Cons       m NodeRef
               , TermBuilder_OLD Curry      m NodeRef
               , TermBuilder_OLD Lam        m NodeRef
               , Connectible NodeRef NodeRef m
               )

runBuilder :: NetworkBuilderT AST m (KnownTypeT ELEMENT NodeRef n) => Builder.NetworkBuilderT m a -> AST -> n (a, AST)
runBuilder cmd ast = runInferenceT ELEMENT (Proxy :: Proxy NodeRef)
                   $ runNetworkBuilderT ast
                   $ Builder.runNetworkBuilderT cmd

runGraph :: (Monad n, NetworkBuilderT AST m (LocationT (KnownTypeT ELEMENT NodeRef n))) => Builder.NetworkBuilderT (ErrorT Error m) a -> AST -> n (Either Error a, AST)
runGraph cmd g = runInferenceT ELEMENT (Proxy :: Proxy NodeRef)
               $ flip Location.evalT Nothing
               $ runNetworkBuilderT g
               $ runErrorT
               $ Builder.runNetworkBuilderT
               $ cmd

runASTOp :: NetworkBuilderT AST m (LocationT (KnownTypeT ELEMENT NodeRef IO)) => Builder.NetworkBuilderT (ErrorT Error m) a -> Command AST a
runASTOp = empire . const . runGraph
