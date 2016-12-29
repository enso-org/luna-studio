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

import           Empire.API.Data.DefaultValue      (Value (..))
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

import           Empire.Utils.TextResult           (nodeValueToText)

import           Luna.IR.Function.Argument (Arg(..))
import           Luna.IR (match)
import qualified Luna.IR as IR
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

limit :: [a] -> [a]
limit = limitHead where
    limitCount = 1000
    limitHead  = take limitCount

data ValueDecoderRep = ConsRep String
                     | AppRep ValueDecoderRep ValueDecoderRep

valueDecoderRep :: ASTOp m => NodeRef -> m (Maybe ValueDecoderRep)
valueDecoderRep node = match node $ \case
    Cons n -> do
        name <- ASTRead.getName n
        return $ Just $ ConsRep name
    App tc (Arg _ typ) -> do
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

concreteValueDecoder :: ValueDecoderRep -> Maybe (Data -> Value)
concreteValueDecoder rep = case rep of
    ConsRep name ->
        case name of
          "Int"            -> Just $ IntValue       . unsafeFromData
          "String"         -> Just $ StringValue    . unsafeFromData
          "Double"         -> Just $ DoubleValue    . unsafeFromData
          "Bool"           -> Just $ BoolValue      . unsafeFromData
          "Histogram"      -> Just $ Histogram      . unsafeFromData
          "IntPairList"    -> Just $ IntPairList    . unsafeFromData
          "DoublePairList" -> Just $ DoublePairList . unsafeFromData
          {-"Graphics"       -> Just $ Graphics       $ fromGraphics     v-}
          {-"Layer"          -> Just $ Graphics       $ fromLayer        v-}
          {-"Geometry"       -> Just $ Graphics       $ fromGeometry     v-}
          {-"GeoComponent"   -> Just $ Graphics       $ fromGeoComponent v-}
          {-"Surface"        -> Just $ Graphics       $ fromSurface      v-}
          {-"Shape"          -> Just $ Graphics       $ fromShape        v-}
          {-"Primitive"      -> Just $ Graphics       $ fromPrimitive    v-}
          {-"Figure"         -> Just $ Graphics       $ fromFigure       v-}
          {-"Material"       -> Just $ Graphics       $ fromMaterial     v-}
          "RGBColor"       -> Just $ Graphics . fromMaterial . colorRGBToMaterial . unsafeFromData
          _                -> Nothing
    AppRep (ConsRep "Stream") param -> concreteValueDecoder param
    AppRep (ConsRep "Maybe") param -> case param of
        ConsRep name -> case name of
            "Int"    -> Just $ IntMaybe    . unsafeFromData
            "Double" -> Just $ DoubleMaybe . unsafeFromData
            "Bool"   -> Just $ BoolMaybe   . unsafeFromData
            "String" -> Just $ StringMaybe . unsafeFromData
            _        -> Nothing
        _ -> Nothing
    AppRep (ConsRep "List") param -> case param of
        ConsRep name -> case name of
            "Int"    -> Just $ IntList    . unsafeFromData
            "Double" -> Just $ DoubleList . unsafeFromData
            "Bool"   -> Just $ BoolList   . unsafeFromData
            "String" -> Just $ StringList . unsafeFromData
            _        -> Nothing
        AppRep (ConsRep "Maybe") param' -> case param' of
            ConsRep name -> case name of
                "Int"    -> Just $ StringMaybeList . intMaybeListToStringMaybeList    . unsafeFromData
                "Double" -> Just $ StringMaybeList . doubleMaybeListToStringMaybeList . unsafeFromData
                "Bool"   -> Just $ StringMaybeList . boolMaybeListToStringMaybeList   . unsafeFromData
                "String" -> Just $ StringMaybeList . unsafeFromData
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    AppRep (ConsRep "Map") (AppRep (ConsRep "String") (ConsRep param)) -> case param of
        "Int"    -> Just $ StringStringMap . stringIntMapToStringStringMap    . unsafeFromData
        "Double" -> Just $ StringStringMap . stringDoubleMapToStringStringMap . unsafeFromData
        "Bool"   -> Just $ StringStringMap . stringBoolMapToStringStringMap   . unsafeFromData
        "String" -> Just $ StringStringMap . unsafeFromData
        _        -> Nothing
    _ -> Nothing


valueDecoderForType :: ASTOp m => NodeRef -> m (Maybe (Data -> Value))
valueDecoderForType tpNode = do
    rep <- valueDecoderRep tpNode
    return $ case rep of
        Just r -> concreteValueDecoder r
        _      -> Nothing

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

dumpGraphViz :: ASTOp m => String -> m ()
dumpGraphViz _name = $notImplemented
    -- g <- runASTOp Builder.get
    -- liftIO $ renderAndOpen [(name, name, g)]
