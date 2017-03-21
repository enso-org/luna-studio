{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.AST where

import           Control.Arrow                     (second)
import           Control.Monad.State
import           Data.Function                     (on)
import           Data.List                         (sortBy)
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Text                         as Text
import           Empire.Prelude

import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.Data.AST                   (NodeRef, NotLambdaException(..), NotUnifyException(..))
import           Empire.Data.Layers                (Meta, TypeLayer)

import           Empire.ASTOp                      (ASTOp, match)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Parse               as Parser
import qualified Empire.ASTOps.Print               as Printer
import qualified Empire.ASTOps.Read                as ASTRead

import           Empire.Utils.TextResult           (nodeValueToText)

import qualified Luna.IR as IR
import           Luna.IR.Term.Uni
import qualified OCI.IR.Repr.Vis as Vis

import           Web.Browser                       (openBrowser)

-- TODO: This might deserve rewriting to some more general solution
import qualified Graphics.API                      as G
-- import           Luna.Pass.Evaluation.Interpreter.Charts (autoScatterChartDouble, autoScatterChartDoubleTuple, autoScatterChartInt,
--                                                           autoScatterChartIntTuple)


addNode :: ASTOp m => NodeId -> String -> String -> m NodeRef
addNode nid name expr = do
    node <- Parser.parseExpr expr
    ASTBuilder.makeNodeRep nid name node

limit :: [a] -> [a]
limit = limitHead where
    limitCount = 1000
    limitHead  = take limitCount

data ValueDecoderRep = ConsRep String
                     | AppRep ValueDecoderRep ValueDecoderRep

valueDecoderRep :: ASTOp m => NodeRef -> m (Maybe ValueDecoderRep)
valueDecoderRep node = match node $ \case
    -- FIXME, use this second parameter
    Cons n _ -> do
        name <- pure $ pathNameToString n
        return $ Just $ ConsRep name
    App tc typ -> do
        tc'    <- IR.source tc
        typ'   <- IR.source typ
        tcRep  <- valueDecoderRep tc'
        typRep <- valueDecoderRep typ'
        case tcRep of
            Just r -> case typRep of
                Just s -> return $ Just $ AppRep r s
                _ -> return Nothing
            _ -> return Nothing
    _ -> return Nothing


intMaybeListToStringMaybeList :: [Maybe Int] -> [Maybe String]
intMaybeListToStringMaybeList = fmap (fmap show)

doubleMaybeListToStringMaybeList :: [Maybe Double] -> [Maybe String]
doubleMaybeListToStringMaybeList = fmap (fmap show)

boolMaybeListToStringMaybeList :: [Maybe Bool] -> [Maybe String]
boolMaybeListToStringMaybeList = fmap (fmap show)

stringIntMapToStringStringMap :: [(String, Int)] -> [(String, String)]
stringIntMapToStringStringMap = fmap (second show)

stringDoubleMapToStringStringMap :: [(String, Double)] -> [(String, String)]
stringDoubleMapToStringStringMap = fmap (second show)

stringBoolMapToStringStringMap :: [(String, Bool)] -> [(String, String)]
stringBoolMapToStringStringMap = fmap (second show)

getNodeValue :: (MonadIO m, ASTOp m) => NodeRef -> m (Either String a)
getNodeValue node = do
    tpNode  <- IR.source =<< IR.readLayer @TypeLayer node
    -- decoder <- decoderForType tpNode
    -- v <- (^. Interpreter.value) <$> IR.readLayer @InterpreterData node
    $notImplemented


readMeta :: ASTOp m => NodeRef -> m (Maybe NodeMeta)
readMeta ref = IR.readLayer @Meta ref

writeMeta :: ASTOp m => NodeRef -> NodeMeta -> m ()
writeMeta ref newMeta = IR.writeLayer @Meta (Just newMeta) ref

sortByPosition :: ASTOp m => [NodeId] -> m [NodeRef]
sortByPosition nodeIds = do
    refs  <- mapM ASTRead.getASTPointer nodeIds
    metas <- mapM readMeta refs
    let refsAndMetas = zip refs metas
        sorted       = sortBy (compare `on` snd) refsAndMetas
    return $ map (^. _1) sorted

makeSeq :: ASTOp m => [NodeRef] -> m (Maybe NodeRef)
makeSeq []     = return Nothing
makeSeq [node] = return $ Just node
makeSeq (n:ns) = Just <$> foldM f n ns
    where
        f :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
        f l r = IR.generalize <$> IR.seq l r

readSeq :: ASTOp m => NodeRef -> m [NodeRef]
readSeq node = match node $ \case
    Seq l r -> do
        previous  <- IR.source l >>= readSeq
        rightmost <- IR.source r
        return (previous ++ [rightmost])
    _       -> return [node]

tryHead :: [a] -> Maybe a
tryHead [] = Nothing
tryHead (a:_) = Just a

getError :: ASTOp m => NodeRef -> m (Maybe (APIError.Error TypeRep))
getError n = return $ Nothing
    {-tc <- view tcErrors <$> IR.readLayer @TCData n-}
    {-err <- mapM reprError tc-}
    {-inp <- IR.readLayer @InputsLayer n-}
    {-inps <- mapM (IR.source) inp-}
    {-inpErrs <- catMaybes <$> mapM getError inps-}
    {-return $ tryHead err <|> tryHead inpErrs-}

{-reprError :: ASTOp m => TCError NodeRef -> m (APIError.Error TypeRep)-}
{-reprError tcErr = case tcErr of-}
    {-ImportError Nothing m  -> return $ APIError.ImportError m-}
    {-ImportError (Just n) m -> do-}
        {-tp <- do-}
            {-t <- IR.readLayer @TypeLayer n-}
            {-IR.source t-}
        {-tpRep <- Printer.getTypeRep tp-}
        {-return $ APIError.NoMethodError m tpRep-}
    {-UnificationError uniNode -> do-}
        {-match uniNode $ \case-}
            {-Unify l r -> APIError.TypeError <$> (Printer.getTypeRep =<< IR.source l) <*> (Printer.getTypeRep =<< IR.source r)-}
            {-_         -> throwM $ NotUnifyException uniNode-}

getLambdaInputRef :: ASTOp m => NodeRef -> Int -> m NodeRef
getLambdaInputRef node pos = do
    match node $ \case
        Lam _args _out -> (!! pos) <$> ASTDeconstruct.extractArguments node
        _              -> throwM $ NotLambdaException node

isTrivialLambda :: ASTOp m => NodeRef -> m Bool
isTrivialLambda node = match node $ \case
    Lam{} -> do
        args <- ASTDeconstruct.extractArguments node
        vars <- concat <$> mapM ASTRead.getVarsInside args
        out' <- ASTRead.getLambdaOutputRef node
        return $ out' `elem` vars
    _ -> throwM $ NotLambdaException node

dumpGraphViz :: ASTOp m => String -> m ()
dumpGraphViz name = Vis.snapshot name
