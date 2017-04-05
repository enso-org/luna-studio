{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Empire.ASTOps.Builder (
    buildAccessors
  , lams
  , makeNodeRep
  , makeAccessor
  , applyFunction
  , removeAccessor
  ) where

import           Control.Monad                      (foldM, replicateM, forM_)
import           Data.Maybe                         (isNothing)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.PortRef            (OutPortRef (..))
import           Empire.API.Data.NodeLoc            (NodeLoc (..))
import qualified Empire.API.Data.Port               as Port
import           Empire.ASTOp                       (ASTOp, match)
import           Empire.ASTOps.Deconstruct          (deconstructApp, extractArguments, dumpAccessors)
import           Empire.ASTOps.Remove               (removeSubtree)
import qualified Empire.ASTOps.Read                 as ASTRead
import           Empire.Data.AST                    (NodeRef, astExceptionFromException,
                                                     astExceptionToException)
import           Empire.Data.Layers                 (Marker)

import           Luna.IR.Term.Uni
import qualified Luna.IR as IR


apps :: ASTOp m => IR.Expr f -> [NodeRef] -> m NodeRef
apps fun exprs = IR.unsafeRelayout <$> foldM f (IR.unsafeRelayout fun) (IR.unsafeRelayout <$> exprs)
    where
        f fun' arg' = appAny fun' arg'

appAny :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
appAny = fmap IR.generalize .: IR.app

lams :: ASTOp m => [NodeRef] -> NodeRef -> m NodeRef
lams args output = IR.unsafeRelayout <$> foldM (flip lamAny) (IR.unsafeRelayout output) (IR.unsafeRelayout <$> reverse args)

lamAny :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
lamAny a b = fmap IR.generalize $ IR.lam a b

newApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
newApplication fun arg' pos = do
    blanks <- sequence $ replicate pos IR.blank
    let args = IR.generalize blanks ++ [arg']
    apps fun args

rewireApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
rewireApplication fun arg' pos = do
    (target, oldArgs) <- deconstructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
    blanks <- replicateM (argsLength - length oldArgs) IR.blank
    let argsCmd = oldArgs ++ map IR.generalize blanks
        withNewArg = argsCmd & ix pos .~ arg'

    apps target withNewArg

applyFunction :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
applyFunction fun arg' pos = match fun $ \case
    App{} -> rewireApplication fun arg' pos
    _     -> newApplication fun arg' pos


reapply :: ASTOp m => NodeRef -> [NodeRef] -> m NodeRef
reapply funRef args = do
    funNode <- pure funRef
    fun <- match funNode $ \case
        App t _ -> do
            f <- IR.source t
            removeSubtree funRef
            return f
        _ -> return funRef
    apps fun args

buildAccessors :: ASTOp m => NodeRef -> [String] -> m NodeRef
buildAccessors = foldM $ \t n -> IR.acc' t (stringToName n)

data SelfPortNotExistantException = SelfPortNotExistantException NodeRef
    deriving (Show)

instance Exception SelfPortNotExistantException where
    toException = astExceptionToException
    fromException = astExceptionFromException

makeAccessor :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
makeAccessor target naming = do
    (_, names) <- dumpAccessors naming
    when (null names) $ throwM $ SelfPortNotExistantException naming
    args <- extractArguments naming
    acc <- buildAccessors target names
    if null args then return acc else reapply acc args

data SelfPortNotConnectedException = SelfPortNotConnectedException NodeRef
    deriving (Show)

instance Exception SelfPortNotConnectedException where
    toException = astExceptionToException
    fromException = astExceptionFromException

removeAccessor :: ASTOp m => NodeRef -> m NodeRef
removeAccessor ref = do
    (target, names) <- dumpAccessors ref
    args            <- extractArguments ref
    when (isNothing target) $ throwM $ SelfPortNotConnectedException ref
    case names of
        []     -> throwM $ SelfPortNotConnectedException ref
        n : ns -> do
            v   <- IR.var' $ stringToName n
            acc <- buildAccessors v ns
            if null args then return acc else reapply acc args

attachNodeMarkers :: ASTOp m => NodeId -> NodeRef -> m ()
attachNodeMarkers marker ref = do
    pats <- ASTRead.dumpPatternVars ref
    forM_ (zip pats $ [0 :: Int ..]) $ \(patVar, num) -> do
        IR.putLayer @Marker patVar $ Just $ OutPortRef (NodeLoc def marker) $ Port.Projection num Port.All

makeNodeRep :: ASTOp m => NodeId -> String -> NodeRef -> m NodeRef
makeNodeRep marker name node = do
    (pat, uni) <- match node $ \case
        Unify l r -> (, node) <$> IR.source l
        _         -> do
            var <- IR.var' $ stringToName name
            uni <- IR.unify var node
            return (IR.generalize var, IR.generalize uni)
    attachNodeMarkers marker pat
    return uni
