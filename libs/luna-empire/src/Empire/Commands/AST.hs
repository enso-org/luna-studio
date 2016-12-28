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
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as Text
import           Empire.Prelude

import           Empire.API.Data.DefaultValue      (PortDefault, Value (..))
import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.Data.AST                   (NodeRef, NotLambdaException(..), NotUnifyException(..))
import           Empire.Data.Layers                (Meta, NodeMarker(..), TCData, TCError(..),
                                                    TypeLayer, InputsLayer, tcErrors)

import           Empire.ASTOp                      (ASTOp)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Deconstruct         as ASTDeconstruct
import qualified Empire.ASTOps.Parse               as Parser
import qualified Empire.ASTOps.Print               as Printer
import qualified Empire.ASTOps.Read                as ASTRead
import           Empire.ASTOps.Remove              (removeArg, removeNode)

import           Empire.Utils.TextResult           (nodeValueToText)

import           Luna.IR (match)
import qualified Luna.IR as IR
import qualified Luna.IR.Expr.Combinators as IR (changeSource)
import           Luna.IR.Expr.Term.Uni

import           Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData (..))
import qualified Luna.Pass.Evaluation.Interpreter.Layer as Interpreter
import           Luna.Pass.Evaluation.Interpreter.Value (Data, attachListener, toExceptIO, unsafeFromData)
import qualified Luna.Pass.Evaluation.Interpreter.Value as Value

import           Empire.Commands.Graphics          (fromMaterial)


-- TODO: This might deserve rewriting to some more general solution
import qualified Graphics.API                      as G
import           Luna.Pass.Evaluation.Interpreter.Charts (autoScatterChartDouble, autoScatterChartDoubleTuple, autoScatterChartInt,
                                                          autoScatterChartIntTuple)


addNode :: ASTOp m => NodeId -> String -> String -> m (NodeRef, NodeRef)
addNode nid name expr = do
    (exprName, ref) <- Parser.parseExpr expr
    let name' = fromMaybe name $ fmap Text.unpack exprName
    (,) <$> pure ref <*> ASTBuilder.makeNodeRep (NodeMarker nid) name' ref

addDefault :: ASTOp m => PortDefault -> m NodeRef
addDefault val = Parser.parsePortDefault val

limit :: [a] -> [a]
limit = limitHead where
    limitCount = 1000
    limitHead  = take limitCount

valueDecoderForType :: ASTOp m => NodeRef -> m (Maybe (Data -> Value))
valueDecoderForType tpNode = match tpNode $ \case
    Cons n -> do
        name <- ASTRead.getName n
        case name of
            "Int"            -> return $ Just $ IntValue       . unsafeFromData
            "String"         -> return $ Just $ StringValue    . unsafeFromData
            "Double"         -> return $ Just $ DoubleValue    . unsafeFromData
            "Bool"           -> return $ Just $ BoolValue      . unsafeFromData
            "Histogram"      -> return $ Just $ Histogram      . unsafeFromData
            "IntPairList"    -> return $ Just $ IntPairList    . unsafeFromData
            "DoublePairList" -> return $ Just $ DoublePairList . unsafeFromData
            {-"Graphics"       -> return $ Just $ Graphics       $ fromGraphics     v-}
            {-"Layer"          -> return $ Just $ Graphics       $ fromLayer        v-}
            {-"Geometry"       -> return $ Just $ Graphics       $ fromGeometry     v-}
            {-"GeoComponent"   -> return $ Just $ Graphics       $ fromGeoComponent v-}
            {-"Surface"        -> return $ Just $ Graphics       $ fromSurface      v-}
            {-"Shape"          -> return $ Just $ Graphics       $ fromShape        v-}
            {-"Primitive"      -> return $ Just $ Graphics       $ fromPrimitive    v-}
            {-"Figure"         -> return $ Just $ Graphics       $ fromFigure       v-}
            {-"Material"       -> return $ Just $ Graphics       $ fromMaterial     v-}
            "RGBColor"       -> return $ Just $ Graphics . fromMaterial . colorRGBToMaterial . unsafeFromData
            "Stream"         -> $notImplemented
            "List"           -> $notImplemented
            "Maybe"          -> $notImplemented
            "Map"            -> $notImplemented
            _                -> return Nothing
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

colorRGBToMaterial :: Value.Color -> G.Material
colorRGBToMaterial (Value.Color r g b) =  G.SolidColor r g b 1.0

decoderForType :: ASTOp m => NodeRef -> m (Data -> (Text, [Value]))
decoderForType tpRef = do
    valueDecoder <- valueDecoderForType tpRef
    return $ case valueDecoder of
        Just f -> decorateValue . f
        _      -> const ("", [])

decorateValue :: Value -> (Text, [Value])
decorateValue val = (name, values) where
    name   = nodeValueToText val
    values = case val of
        IntList        list -> let list' = limit list in [IntList        list', Graphics $ autoScatterChartInt         gridMat mat figure scale shift list']
        DoubleList     list -> let list' = limit list in [DoubleList     list', Graphics $ autoScatterChartDouble      gridMat mat figure scale shift list']
        Histogram      list -> let list' = limit list in [Histogram      list', Graphics $ autoScatterChartIntTuple    gridMat mat figure scale shift list']
        IntPairList    list -> let list' = limit list in [IntPairList    list', Graphics $ autoScatterChartIntTuple    gridMat mat figure scale shift list']
        DoublePairList list -> let list' = limit list in [DoublePairList list', Graphics $ autoScatterChartDoubleTuple gridMat mat figure scale shift list']
        _                   -> [val]
        where
            gridMat    = G.SolidColor 0.25 0.25 0.25 1.0
            mat        = G.SolidColor 0.2  0.5  0.7  1.0
            figure     = G.Circle 0.016
            scale      = 0.84
            shift      = 0.05

data ValueRep = PlainVal (Text, [Value]) | Listener (((Text, [Value]) -> IO ()) -> IO (IO ()))

type instance IR.LayerData InterpreterData t = Interpreter.InterpreterLayer

getNodeValue :: (MonadIO m, ASTOp m) => NodeRef -> m (Either String ValueRep)
getNodeValue node = do
    tpNode  <- IR.source =<< IR.readLayer @TypeLayer node
    decoder <- decoderForType tpNode
    v <- (^. Interpreter.value) <$> IR.readLayer @InterpreterData node
    case v of
        Left  _err -> return $ Right $ PlainVal ("", [])
        Right val -> do
            val' <- liftIO . runExceptT $ toExceptIO val
            case val' of
                Left  s -> return $ Left s
                Right v' -> match tpNode $ \case
                    Cons n -> do
                        name <- ASTRead.getName n
                        case name of
                            "Stream"  -> return $ Right $ Listener $ \f -> attachListener (unsafeFromData v') (f . decoder)
                            "Twitter" -> return $ Right $ Listener $ \_ -> attachListener (unsafeFromData v') (const $ return ())
                            _         -> return $ Right $ PlainVal $ decoder v'
                    _ -> return $ Right $ PlainVal ("", [])

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

removeSubtree :: ASTOp m => NodeRef -> m ()
removeSubtree = removeNode

redirectLambdaOutput :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
redirectLambdaOutput lambda newOutputRef = do
    match lambda $ \case
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            ASTBuilder.lams args' newOutputRef
        _ -> throwM $ NotLambdaException lambda

setLambdaOutputToBlank :: ASTOp m => NodeRef -> m NodeRef
setLambdaOutputToBlank lambda = do
    match lambda $ \case
        Lam _args _ -> do
            args' <- ASTDeconstruct.extractArguments lambda
            blank <- IR.generalize <$> IR.blank
            ASTBuilder.lams args' blank
        _ -> throwM $ NotLambdaException lambda

removeAccessor :: ASTOp m => NodeRef -> m NodeRef
removeAccessor = ASTBuilder.unAcc

getLambdaInputRef :: ASTOp m => NodeRef -> Int -> m NodeRef
getLambdaInputRef node pos = do
    match node $ \case
        Lam _args _out -> (!! pos) <$> ASTDeconstruct.extractArguments node
        _ -> throwM $ NotLambdaException node

isTrivialLambda :: ASTOp m => NodeRef -> m Bool
isTrivialLambda node = match node $ \case
    Lam _args out -> do
        args <- ASTDeconstruct.extractArguments node
        out' <- IR.source out
        return $ out' `elem` args
    _ -> throwM $ NotLambdaException node

getLambdaOutputRef :: ASTOp m => NodeRef -> m NodeRef
getLambdaOutputRef lambda = do
    match lambda $ \case
        Lam _ out -> IR.source out
        _ -> throwM $ NotLambdaException lambda

replaceTargetNode :: ASTOp m => NodeRef -> NodeRef -> m ()
replaceTargetNode matchNode newTarget = do
    match matchNode $ \case
        Unify _l r -> do
            IR.changeSource (IR.generalize r) newTarget
        _ -> throwM $ NotUnifyException matchNode

dumpGraphViz :: ASTOp m => String -> m ()
dumpGraphViz _name = $notImplemented
    -- g <- runASTOp Builder.get
    -- liftIO $ renderAndOpen [(name, name, g)]
