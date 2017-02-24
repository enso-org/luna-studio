{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.AST where

import           Control.Arrow                     (second)
import           Control.Monad.Except              (runExceptT)
import           Control.Monad.State
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy.Char8        as BS.C8
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Empire.Prelude

import           Empire.API.Data.DefaultValue      (Value (..))
import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.Data.AST                   (NodeRef, NotLambdaException(..), NotUnifyException(..))
import           Empire.Data.Layers                (Meta, NodeMarker(..), TCData, TCError(..),
                                                    TypeLayer, InputsLayer, tcErrors)

import           Empire.ASTOp                      (ASTOp, match)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Parse               as Parser
import qualified Empire.ASTOps.Print               as Printer
import qualified Empire.ASTOps.Read                as ASTRead

import           Empire.Utils.TextResult           (nodeValueToText)

import qualified Luna.IR as IR
import           Luna.IR.Expr.Term.Uni
import qualified Luna.IR.Repr.Vis as Vis

import           Web.Browser                       (openBrowser)

-- TODO: This might deserve rewriting to some more general solution
import qualified Graphics.API                      as G
-- import           Luna.Pass.Evaluation.Interpreter.Charts (autoScatterChartDouble, autoScatterChartDoubleTuple, autoScatterChartInt,
--                                                           autoScatterChartIntTuple)


addNode :: ASTOp m => NodeId -> String -> String -> m (NodeRef, NodeRef)
addNode nid name expr = do
    (exprName, ref) <- Parser.parseExpr expr
    let name' = fromMaybe name $ fmap Text.unpack exprName
    (,) <$> pure ref <*> ASTBuilder.makeNodeRep (NodeMarker nid) name' ref

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
        name <- ASTRead.getName n
        return $ Just $ ConsRep name
    App tc typ -> do
        tc' <- IR.source tc
        typ' <- IR.source typ
        tcRep <- valueDecoderRep tc'
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

tryHead :: [a] -> Maybe a
tryHead [] = Nothing
tryHead (a:_) = Just a

getError :: ASTOp m => NodeRef -> m (Maybe (APIError.Error TypeRep))
getError n = do
    tc <- view tcErrors <$> IR.readLayer @TCData n
    err <- mapM reprError tc
    inp <- IR.readLayer @InputsLayer n
    inps <- mapM (IR.source) inp
    inpErrs <- catMaybes <$> mapM getError inps
    return $ tryHead err <|> tryHead inpErrs

reprError :: ASTOp m => TCError NodeRef -> m (APIError.Error TypeRep)
reprError tcErr = case tcErr of
    ImportError Nothing m  -> return $ APIError.ImportError m
    ImportError (Just n) m -> do
        tp <- do
            t <- IR.readLayer @TypeLayer n
            IR.source t
        tpRep <- Printer.getTypeRep tp
        return $ APIError.NoMethodError m tpRep
    UnificationError uniNode -> do
        match uniNode $ \case
            Unify l r -> APIError.TypeError <$> (Printer.getTypeRep =<< IR.source l) <*> (Printer.getTypeRep =<< IR.source r)
            _         -> throwM $ NotUnifyException uniNode

writeMeta :: ASTOp m => NodeRef -> NodeMeta -> m ()
writeMeta ref newMeta = IR.writeLayer @Meta (Just newMeta) ref

getLambdaInputRef :: ASTOp m => NodeRef -> Int -> m NodeRef
getLambdaInputRef node pos = do
    match node $ \case
        Lam _args _out -> (!! pos) <$> ASTDeconstruct.extractArguments node
        _ -> throwM $ NotLambdaException node

isTrivialLambda :: ASTOp m => NodeRef -> m Bool
isTrivialLambda node = match node $ \case
    Lam{} -> do
        args <- ASTDeconstruct.extractArguments node
        out' <- ASTRead.getLambdaOutputRef node
        return $ out' `elem` args
    _ -> throwM $ NotLambdaException node

dumpGraphViz :: ASTOp m => String -> m ()
dumpGraphViz name = do
    return ()
    -- ((), diff) <- Vis.newRunDiffT $ Vis.snapshot name
    -- let vis = BS.C8.unpack $ Aeson.encode $ diff
    -- void $ liftIO $ openBrowser $ "http://localhost:8000?cfg=" <> vis
