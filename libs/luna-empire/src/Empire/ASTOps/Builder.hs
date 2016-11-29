{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Builder where

import           Control.Monad                      (foldM)
import           Control.Monad.Error                (throwError)
import           Data.Direction                     (source)
import qualified Data.HMap.Lazy                     as HMap
import           Data.Layer_OLD.Cover_OLD           (covered, uncover)
import           Data.List                          (dropWhileEnd)
import           Data.Maybe                         (isJust, isNothing)
import           Old.Data.Prop                      (prop, ( # ))
import           Data.Record                        (ANY (..), caseTest, cons, of')
import           Prologue                           hiding (cons, ( # ))

import           Empire.API.Data.Node               (NodeId)
import           Empire.ASTOp                       (ASTOp)
import           Empire.ASTOps.Remove               (removeNode)
import           Empire.Data.AST                    (ASTNode, EdgeRef, NodeRef, UncoveredNode)
import           Empire.Data.NodeMarker             (NodeMarker, nodeMarkerKey)
import           Old.Luna.Syntax.Term.Class         (Acc (..), App (..), Blank (..), Curry (..), Match (..), Var (..))
import qualified Old.Luna.Syntax.Term.Expr.Lit      as Lit

import           Old.Luna.Syntax.Model.Network.Builder  (Meta (..))
import qualified Old.Luna.Syntax.Model.Network.Builder  as Builder
import           Luna.IR.Function.Argument (Arg)
import qualified Luna.IR.Function.Argument as Arg

functionApplicationNode :: Lens' ASTNode EdgeRef
functionApplicationNode = covered . lens getter setter where
    getter a   = caseTest a $ of' $ \(App f _   ) -> f
    setter a f = caseTest a $ of' $ \(App _ args) -> cons $ App f args

accessorTarget :: Lens' ASTNode EdgeRef
accessorTarget = covered . lens getter setter where
    getter (a :: UncoveredNode)   = caseTest a $ of' $ \(Acc _ t) -> t :: EdgeRef
    setter (a :: UncoveredNode) t = caseTest a $ of' $ \(Acc n _) -> cons $ Acc n t :: UncoveredNode

unpackArguments :: ASTOp m => [Arg EdgeRef] -> m [NodeRef]
unpackArguments args = mapM (Builder.follow source . Arg.__val_) args

isApp :: ASTOp m => NodeRef -> m Bool
isApp ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(App _ _) -> return True
        of' $ \ANY   -> return False

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \Blank -> return True
        of' $ \ANY   -> return False

removeArg :: ASTOp m => NodeRef -> Int -> m NodeRef
removeArg fun pos = do
    (f, args)  <- destructApp fun
    freshBlank <- Builder.blank
    let newArgs = args & ix pos .~ freshBlank
    areBlanks <- mapM isBlank newArgs
    let argsLength = length $ dropWhileEnd id areBlanks
    Builder.app f (Builder.arg <$> (take argsLength newArgs))

destructApp :: ASTOp m => NodeRef -> m (NodeRef, [NodeRef])
destructApp fun = do
    app    <- Builder.read fun
    result <- caseTest (uncover app) $ do
        of' $ \(App tg args) -> do
            unpackedArgs <- unpackArguments args
            target <- Builder.follow source tg
            return (target, unpackedArgs)
        of' $ \ANY -> throwError "Expected App node, got wrong type."
    return result

newApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
newApplication fun arg pos = do
    blanks <- sequence $ replicate pos Builder.blank
    let args = blanks ++ [arg]
    Builder.app fun (Builder.arg <$> args)

rewireApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
rewireApplication fun arg pos = do
    (target, oldArgs) <- destructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
        argsCmd    = take argsLength $ (return <$> oldArgs) ++ (repeat Builder.blank)
        oldArgCmd  = argsCmd !! pos
        withNewArg = argsCmd & ix pos .~ return arg

    args <- sequence withNewArg
    oldArg <- oldArgCmd

    Builder.app target (Builder.arg <$> args)

applyFunction :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
applyFunction fun arg pos = do
    funNode <- Builder.read fun
    caseTest (uncover funNode) $ do
        of' $ \(App _ _) -> rewireApplication fun arg pos
        of' $ \ANY -> newApplication fun arg pos

reapply :: ASTOp m => NodeRef -> [NodeRef] -> m NodeRef
reapply funRef args = do
    funNode <- Builder.read funRef
    fun <- caseTest (uncover funNode) $ do
        of' $ \(App t _) -> do
            f <- Builder.follow source t
            removeNode funRef
            return f
        of' $ \ANY -> return funRef
    Builder.app fun $ Builder.arg <$> args


dumpAccessors' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors' firstApp ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(Var (Lit.String name)) -> do
            isNode <- isGraphNode ref
            if isNode
                then return (Just ref, [])
                else return (Nothing, [name])
        of' $ \(App t a) -> do
            if not firstApp && not (null a)
                then return (Just ref, [])
                else do
                    target <- Builder.follow source t
                    dumpAccessors' False target
        of' $ \(Acc (Lit.String name) t) -> do
            target <- Builder.follow source t
            (tgt, names) <- dumpAccessors' False target
            return (tgt, names ++ [name])
        of' $ \ANY -> return (Just ref, [])

dumpAccessors :: ASTOp m => NodeRef -> m (Maybe NodeRef, [String])
dumpAccessors = dumpAccessors' True

dumpArguments :: ASTOp m => NodeRef -> m [NodeRef]
dumpArguments ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(App _ as) -> unpackArguments as
        of' $ \ANY        -> return []

buildAccessors :: ASTOp m => NodeRef -> [String] -> m NodeRef
buildAccessors = foldM $ \t n -> Builder.acc (fromString n) t >>= flip Builder.app []

applyAccessors :: ASTOp m => NodeRef -> m NodeRef
applyAccessors = applyAccessors' False

-- FIXME[MK]: move to TC pass
applyAccessors' :: ASTOp m => Bool -> NodeRef -> m NodeRef
applyAccessors' apped ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(Var _) -> if apped then return ref else Builder.app ref []
        of' $ \(Acc n t) -> do
            tgt   <- Builder.follow source t
            isLam <- isBlank tgt
            if isLam
                then return ref
                else do
                    trep <- applyAccessors' False tgt
                    newAcc <- Builder.acc n trep
                    if apped then return newAcc else Builder.app newAcc []
        of' $ \(App f as) -> do
        -- FIXME[MK]: this clause is identical to the curry one. And it happens often. Maybe curry is a wrong abstraction? How to unify it with App? Susp?
            fr   <- Builder.follow source f
            args <- unpackArguments as
            frep <- applyAccessors' True fr
            argReps <- mapM (applyAccessors' False) args
            Builder.app frep (Builder.arg <$> argReps)
        of' $ \(Curry f as) -> do
            fr   <- Builder.follow source f
            args <- unpackArguments as
            frep <- applyAccessors' True fr
            argReps <- mapM (applyAccessors' False) args
            Builder.curry frep (Builder.arg <$> argReps)
        of' $ \ANY -> return ref


makeAccessor :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
makeAccessor target naming = do
    (oldTarget, names) <- dumpAccessors naming
    when (null names) $ throwError "Requested target doesn't have a self port"
    args <- dumpArguments naming
    acc <- buildAccessors target names
    if null args then return acc else reapply acc args

unAcc :: ASTOp m => NodeRef -> m NodeRef
unAcc ref = do
    (target, names) <- dumpAccessors ref
    args            <- dumpArguments ref
    when (isNothing target) $ throwError "Self port not connected"
    case names of
        []     -> throwError "Self port not connected"
        n : ns -> do
            v   <- Builder.var (fromString n)
            acc <- buildAccessors v ns
            if null args then return acc else reapply acc args

makeNodeRep :: forall m. ASTOp m => NodeMarker -> String -> NodeRef -> m NodeRef
makeNodeRep marker name node = do
    nameVar <- Builder.var (fromString name) :: m NodeRef
    Builder.withRef nameVar $ prop Meta %~ HMap.insert nodeMarkerKey marker
    Builder.match nameVar node

rightMatchOperand :: Lens' ASTNode (EdgeRef)
rightMatchOperand = covered . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ of' $ \(Match _ r) -> r
    rightSetter u r = caseTest u $ of' $ \(Match l _) -> cons $ Match l r

leftMatchOperand :: Lens' ASTNode (EdgeRef)
leftMatchOperand = covered . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ of' $ \(Match l _) -> l
    rightSetter u l = caseTest u $ of' $ \(Match _ r) -> cons $ Match l r

varName :: Lens' ASTNode String
varName = covered . lens nameGetter nameSetter where
    nameGetter v   = caseTest v $ of' $ \(Var (n :: Lit.String)) -> toString n
    nameSetter v n = caseTest v $ of' $ \(Var (_ :: Lit.String)) -> (cons $ Var $ (fromString n :: Lit.String))

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = do
    node    <- Builder.read vref
    Builder.write vref $ node & varName .~ name

isGraphNode :: ASTOp m => NodeRef -> m Bool
isGraphNode = fmap isJust . getNodeId

getNodeId :: ASTOp m => NodeRef -> m (Maybe NodeId)
getNodeId ref = do
    node <- Builder.read ref
    return $ fmap unwrap $ HMap.lookup nodeMarkerKey $ node # Meta
