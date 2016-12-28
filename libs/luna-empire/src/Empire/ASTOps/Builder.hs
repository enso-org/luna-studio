{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Empire.ASTOps.Builder where

import           Control.Monad                      (foldM, replicateM)
import           Data.Coerce                        (coerce)
import           Data.Maybe                         (isJust, isNothing)
import           Empire.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.ASTOps.Deconstruct          (deconstructApp, dumpArguments)
import           Empire.ASTOps.Remove               (removeNode)
import           Empire.Data.AST                    (EdgeRef, NodeRef, NotAppException(..),
                                                     NotUnifyException(..), astExceptionFromException,
                                                     astExceptionToException)
import           Empire.Data.Layers                 (NodeMarker(..), Marker)

import Luna.IR.Expr.Term (Term(Sym_String))
import Luna.IR.Expr.Term.Uni
import Luna.IR.Function (arg)
import           Luna.IR.Function.Argument (Arg)
import qualified Luna.IR.Function.Argument as Arg
import           Luna.IR (match)
import qualified Luna.IR as IR

functionApplicationNode :: ASTOp m => NodeRef -> m EdgeRef
functionApplicationNode node = match node $ \case
    App f _ -> pure f

accessorTarget :: ASTOp m => NodeRef -> m EdgeRef
accessorTarget node = match node $ \case
    Acc _ t -> pure t

unpackArguments :: ASTOp m => [Arg EdgeRef] -> m [NodeRef]
unpackArguments args = mapM (IR.source . Arg.__val_) args

isApp :: ASTOp m => NodeRef -> m Bool
isApp expr = match expr $ \case
    App{} -> return True
    _     -> return False

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank expr = match expr $ \case
    Blank{} -> return True
    _       -> return False

getVarName :: ASTOp m => NodeRef -> m String
getVarName node = match node $ \case
    Var n -> getName n

getName :: ASTOp m => EdgeRef -> m String
getName node = do
    str <- IR.source node
    match str $ \case
        IR.String s -> return s

removeArg :: ASTOp m => NodeRef -> Int -> m NodeRef
removeArg expr i = match expr $ \case
    App a (Arg.Arg _ c) -> do
        nextApp <- IR.source a
        if i == 0 then do
            b  <- IR.blank
            IR.generalize <$> IR.app nextApp (arg b)
        else do
            d <- IR.source c
            f <- removeArg nextApp (i - 1)
            IR.generalize <$> IR.app f (arg d)
    _       -> throwM $ NotAppException expr


apps :: ASTOp m => IR.Expr f -> [NodeRef] -> m NodeRef
apps fun exprs = IR.unsafeRelayout <$> foldM f (IR.unsafeRelayout fun) (IR.unsafeRelayout <$> exprs)
    where
        f fun' arg' = appAny fun' (arg arg')

appAny :: ASTOp m => NodeRef -> Arg (NodeRef) -> m NodeRef
appAny = fmap IR.generalize .: IR.app

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
            removeNode funRef
            return f
        _ -> return funRef
    apps fun args


dumpAccessors' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors' firstApp ref = do
    node <- pure ref
    match node $ \case
        Var n -> do
            isNode <- isGraphNode ref
            name <- getName n
            if isNode
                then return (Just ref, [])
                else return (Nothing, [name])
        App t a -> do
            if not firstApp && not (null a)
                then return (Just ref, [])
                else do
                    target <- IR.source t
                    dumpAccessors' False target
        Acc n t -> do
            target <- IR.source t
            name <- getName n
            (tgt, names) <- dumpAccessors' False target
            return (tgt, names ++ [name])
        _ -> return (Just ref, [])

dumpAccessors :: ASTOp m => NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors = dumpAccessors' True

buildAccessors :: ASTOp m => NodeRef -> [String] -> m NodeRef
buildAccessors = foldM $ \t n -> IR.rawAcc n t >>= flip apps []

applyAccessors :: ASTOp m => NodeRef -> m NodeRef
applyAccessors = applyAccessors' False

-- FIXME[MK]: move to TC pass
applyAccessors' :: ASTOp m => Bool -> NodeRef -> m NodeRef
applyAccessors' apped node = match node $ \case
    Var _ -> if apped then return node else apps node []
    Acc n t -> do
        name  <- IR.source n
        tgt   <- IR.source t
        isLam <- isBlank tgt
        if isLam
            then return node
            else do
                trep <- applyAccessors' False tgt
                newAcc <- IR.generalize <$> IR.acc name trep
                if apped then return newAcc else apps newAcc []
    App f _as -> do
    -- FIXME[MK]: this clause is identical to the curry one. And it happens often. Maybe curry is a wrong abstraction? How to unify it with App? Susp?
        fr   <- IR.source f
        args <- dumpArguments node
        frep <- applyAccessors' True fr
        argReps <- mapM (applyAccessors' False) args
        apps frep argReps
    _ -> return node

data SelfPortNotExistantException = SelfPortNotExistantException NodeRef
    deriving (Show)

instance Exception SelfPortNotExistantException where
    toException = astExceptionToException
    fromException = astExceptionFromException

makeAccessor :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
makeAccessor target naming = do
    (_, names) <- dumpAccessors naming
    when (null names) $ throwM $ SelfPortNotExistantException naming
    args <- dumpArguments naming
    acc <- buildAccessors target names
    if null args then return acc else reapply acc args

data SelfPortNotConnectedException = SelfPortNotConnectedException NodeRef
    deriving (Show)

instance Exception SelfPortNotConnectedException where
    toException = astExceptionToException
    fromException = astExceptionFromException

unAcc :: ASTOp m => NodeRef -> m NodeRef
unAcc ref = do
    (target, names) <- dumpAccessors ref
    args            <- dumpArguments ref
    when (isNothing target) $ throwM $ SelfPortNotConnectedException ref
    case names of
        []     -> throwM $ SelfPortNotConnectedException ref
        n : ns -> do
            v   <- IR.generalize <$> IR.strVar n
            acc <- buildAccessors v ns
            if null args then return acc else reapply acc args

makeNodeRep :: ASTOp m => NodeMarker -> String -> NodeRef -> m NodeRef
makeNodeRep marker name node = do
    (nameVar :: NodeRef) <- IR.generalize <$> IR.strVar name
    IR.writeLayer @Marker (Just marker) nameVar
    IR.generalize <$> IR.unify nameVar node

rightMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
rightMatchOperand node = match node $ \case
    Unify _ b -> pure b
    _         -> throwM $ NotUnifyException node

leftMatchOperand :: ASTOp m => NodeRef -> m EdgeRef
leftMatchOperand node = match node $ \case
    Unify a _ -> pure a
    _         -> throwM $ NotUnifyException node

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = match vref $ \case
    Var n -> do
        (var :: IR.Expr (IR.E IR.String)) <- IR.unsafeGeneralize <$> IR.source n
        IR.modifyExprTerm var $ IR.lit .~ name

isGraphNode :: ASTOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
getNodeId node = coerce <$> IR.readLayer @Marker node
