module Empire.ASTOps.Deconstruct where

import           Empire.Prelude

import           Data.Text.Position (Delta)
import           Empire.ASTOp       (GraphOp, match)
import qualified Empire.ASTOps.Read as Read
import           Empire.Data.AST    (EdgeRef, NodeRef, NotAppException (..))
import           Luna.Pass.Data.Layer.SpanLength (SpanLength)
import           Luna.Pass.Data.Layer.SpanOffset (SpanOffset)


deconstructApp :: NodeRef -> GraphOp (NodeRef, [NodeRef])
deconstructApp app' = match app' $ \case
    Grouped g -> deconstructApp =<< source g
    App{}     -> do
        unpackedArgs <- extractArguments app'
        tgt          <- extractFun app'
        return (tgt, unpackedArgs)
    _ -> throwM $ NotAppException app'

extractFun :: NodeRef -> GraphOp NodeRef
extractFun app = match app $ \case
    App a _   -> extractFun =<< source a
    Grouped g -> extractFun =<< source g
    _ -> return app

extractSelf :: NodeRef -> GraphOp (Maybe NodeRef)
extractSelf ref = match ref $ \case
    Acc s _   -> Just <$> source s
    Grouped g -> extractSelf =<< source g
    _         -> return Nothing

data ExtractFilter = FApp | FLam

extractArguments :: NodeRef -> GraphOp [NodeRef]
extractArguments expr = match expr $ \case
    App{}       -> reverse <$> extractArguments' FApp expr
    Lam{}       -> extractArguments' FLam expr
    Cons _ args -> mapM source =<< ptrListToList args
    Grouped g   -> source g >>= extractArguments
    _           -> return []

extractLamArguments :: NodeRef -> GraphOp [NodeRef]
extractLamArguments = extractArguments' FLam

extractFunctionPorts :: NodeRef -> GraphOp [NodeRef]
extractFunctionPorts ref = matchExpr ref $ \case
    ASGFunction _ as _ -> mapM source =<< ptrListToList as
    Lam i o            -> (:) <$> source i <*> (extractFunctionPorts =<< source o)
    Grouped g          -> extractFunctionPorts =<< source g
    _                  -> return []

extractAppArguments :: NodeRef -> GraphOp [NodeRef]
extractAppArguments = extractArguments' FApp

extractAppPorts :: NodeRef -> GraphOp [NodeRef]
extractAppPorts expr = matchExpr expr $ \case
    Tuple elts -> mapM source =<< ptrListToList elts
    List elts  -> mapM source =<< ptrListToList elts
    _          -> reverse <$> extractAppArguments expr

extractArguments' :: ExtractFilter -> NodeRef -> GraphOp [NodeRef]
extractArguments' FApp expr = match expr $ \case
    App a b -> do
        nextApp <- source a
        args    <- extractArguments' FApp nextApp
        arg'    <- source b
        return $ arg' : args
    Grouped g -> source g >>= extractArguments' FApp
    Acc _ n -> source n >>= extractArguments' FApp
    RightSection _op n -> source n >>= \a -> pure [a]
    LeftSection  _op n -> source n >>= \a -> pure [a]
    _       -> return []
extractArguments' FLam expr = match expr $ \case
    Lam b a -> do
        nextLam <- source a
        args    <- extractArguments' FLam nextLam
        arg'    <- source b
        return $ arg' : args
    Grouped g -> source g >>= extractArguments' FLam
    _       -> return []

extractLamArgLinks :: NodeRef -> GraphOp [(Delta, EdgeRef)]
extractLamArgLinks = extractLamArgLinks' 0

extractLamArgLinks' :: Delta -> NodeRef -> GraphOp [(Delta, EdgeRef)]
extractLamArgLinks' lamOff expr = match expr $ \case
    Lam b a -> do
        off     <- getLayer @SpanOffset a
        nextLam <- source a
        argLen  <- getLayer @SpanLength =<< source b
        args    <- extractLamArgLinks' (lamOff + argLen + off) nextLam
        return $ (lamOff, generalize b) : args
    Grouped g -> source g >>= extractLamArgLinks' lamOff
    _       -> return []

dumpAccessors :: NodeRef -> GraphOp (Maybe NodeRef, [String])
dumpAccessors = dumpAccessors' True

dumpAccessors' :: Bool -> NodeRef -> GraphOp (Maybe NodeRef, [String])
dumpAccessors' _firstApp node = do
    match node $ \case
        Var{}  -> do
            isNode <- Read.isGraphNode node
            name   <- Read.getVarName node
            if isNode
                then return (Just node, [])
                else return (Nothing, [name])
        App t _ -> do
            tgt <- source t
            dumpAccessors' False tgt
        Acc t n -> do
            tgt           <- source t
            name          <- Read.getVarName =<< source n
            (tgt', names) <- dumpAccessors' False tgt
            return (tgt', names <> [name])
        _ -> return (Just node, [])
