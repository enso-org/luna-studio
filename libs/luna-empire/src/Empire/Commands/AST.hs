{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Commands.AST where

import           Control.Arrow                     (second)
import           Control.Monad.Except              (runExceptT, throwError)
import           Control.Monad.State
import           Data.Graph                        (Inputs (..), source)
import           Data.HMap.Lazy                    (TypeKey (..))
import qualified Data.HMap.Lazy                    as HMap
import           Data.List                         (find)
import           Data.Layer_OLD.Cover_OLD          (uncover)
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Old.Data.Prop                     (prop, ( # ))
import           Data.Record                       (ANY (..), caseTest, of')
import qualified Data.Text.Lazy                    as Text
import           Prologue                          hiding (( # ))

import           Empire.API.Data.DefaultValue      (PortDefault, Value (..))
import qualified Empire.API.Data.Error             as APIError
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.Data.AST                   (AST, ASTNode, NodeRef)
import           Empire.Data.NodeMarker            (NodeMarker (..))
import           Empire.Empire

import           Empire.ASTOp                      (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Parse               as Parser
import qualified Empire.ASTOps.Print               as Printer
import           Empire.ASTOps.Remove              (safeRemove)

import           Empire.Utils.TextResult           (nodeValueToText)

import           Old.Luna.Pretty.GraphViz          (renderAndOpen)

import           Old.Luna.Syntax.Model.Network.Builder (Meta (..), TCData (..), Type (..), tcErrors)
import qualified Old.Luna.Syntax.Model.Network.Builder as Builder
import           Old.Luna.Syntax.Term.Class        (Cons (..), Unify (..), Lam(..))
import qualified Old.Luna.Syntax.Term.Expr.Lit     as Lit

import           Old.Luna.Compilation.Error            as TCError

import           Luna.Pass.Evaluation.Interpreter.Layer (InterpreterData (..))
import qualified Luna.Pass.Evaluation.Interpreter.Layer as Interpreter
import           Luna.Pass.Evaluation.Interpreter.Value (Data, attachListener, toExceptIO, unsafeFromData)
import qualified Luna.Pass.Evaluation.Interpreter.Value as Value

import           Empire.Commands.Graphics          (fromMaterial)


-- TODO: This might deserve rewriting to some more general solution
import qualified Graphics.API                      as G
import           Luna.Pass.Evaluation.Interpreter.Charts (autoScatterChartDouble, autoScatterChartDoubleTuple, autoScatterChartInt,
                                                          autoScatterChartIntTuple)

metaKey :: TypeKey NodeMeta
metaKey = TypeKey

addNode :: NodeId -> String -> String -> Command AST (NodeRef, NodeRef)
addNode nid name expr = runASTOp $ do
    (exprName, ref) <- Parser.parseExpr expr
    let name' = fromMaybe name $ fmap Text.unpack exprName
    (,) <$> pure ref <*> ASTBuilder.makeNodeRep (NodeMarker nid) name' ref

addDefault :: PortDefault -> Command AST NodeRef
addDefault val = runASTOp $ Parser.parsePortDefault val

limit :: [a] -> [a]
limit = limitHead where
    limitCount = 1000
    limitHead  = take limitCount
    limitTail  = reverse . take limitCount . reverse

valueDecoderForType :: ASTOp m => NodeRef -> m (Maybe (Data -> Value))
valueDecoderForType tp = do
    tpNode <- Builder.read tp
    caseTest (uncover tpNode) $ do
        of' $ \(Cons (Lit.String n) as) -> case n of
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
            "Stream"         -> do
                args <- ASTBuilder.unpackArguments as
                case args of
                    [a] -> valueDecoderForType a
                    _   -> return Nothing
            "List"           -> do
                args <- ASTBuilder.unpackArguments as
                case args of
                    [a] -> do
                        arg <- Builder.read a
                        caseTest (uncover arg) $ do
                            of' $ \(Cons (Lit.String n) as) -> case n of
                                "Int"    -> return $ Just $ IntList    . unsafeFromData
                                "Double" -> return $ Just $ DoubleList . unsafeFromData
                                "Bool"   -> return $ Just $ BoolList   . unsafeFromData
                                "String" -> return $ Just $ StringList . unsafeFromData
                                "Maybe"  -> do
                                    args <- ASTBuilder.unpackArguments as
                                    case args of
                                        [a] -> do
                                            arg <- Builder.read a
                                            caseTest (uncover arg) $ do
                                                of' $ \(Cons (Lit.String n) _) -> case n of
                                                    "Int"    -> return $ Just $ StringMaybeList . intMaybeListToStringMaybeList    . unsafeFromData
                                                    "Double" -> return $ Just $ StringMaybeList . doubleMaybeListToStringMaybeList . unsafeFromData
                                                    "Bool"   -> return $ Just $ StringMaybeList . boolMaybeListToStringMaybeList   . unsafeFromData
                                                    "String" -> return $ Just $ StringMaybeList . unsafeFromData
                                                    _        -> return Nothing
                                                of' $ \ANY -> return Nothing
                                _        -> return Nothing
                            of' $ \ANY -> return Nothing
                    _ -> return Nothing
            "Maybe"           -> do
                args <- ASTBuilder.unpackArguments as
                case args of
                    [a] -> do
                        arg <- Builder.read a
                        caseTest (uncover arg) $ do
                            of' $ \(Cons (Lit.String n) _) -> case n of
                                "Int"    -> return $ Just $ IntMaybe    . unsafeFromData
                                "Double" -> return $ Just $ DoubleMaybe . unsafeFromData
                                "Bool"   -> return $ Just $ BoolMaybe   . unsafeFromData
                                "String" -> return $ Just $ StringMaybe . unsafeFromData
                                _        -> return Nothing
                            of' $ \ANY -> return Nothing
                    _ -> return Nothing
            "Map"           -> do
                args <- ASTBuilder.unpackArguments as
                case args of
                    [k, v] -> do
                        argKey <- Builder.read k
                        argVal <- Builder.read v
                        caseTest (uncover argKey) $ do
                            of' $ \(Cons (Lit.String n) _) -> case n of
                                "String" -> caseTest (uncover argVal) $ do
                                    of' $ \(Cons (Lit.String n) _) -> case n of
                                        "Int"    -> return $ Just $ StringStringMap . stringIntMapToStringStringMap    . unsafeFromData
                                        "Double" -> return $ Just $ StringStringMap . stringDoubleMapToStringStringMap . unsafeFromData
                                        "Bool"   -> return $ Just $ StringStringMap . stringBoolMapToStringStringMap   . unsafeFromData
                                        "String" -> return $ Just $ StringStringMap . unsafeFromData
                                        _        -> return Nothing
                                    of' $ \ANY -> return Nothing
                                _        -> return Nothing
                            of' $ \ANY -> return Nothing
                    _ -> return Nothing
            _ -> return Nothing
        of' $ \ANY -> return Nothing

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

getNodeValue :: NodeRef -> Command AST (Either String ValueRep)
getNodeValue ref = runASTOp $ do
    node    <- Builder.read ref
    tp      <- Builder.follow source $ node ^. prop Type
    tpNode  <- Builder.read tp
    decoder <- decoderForType tp
    case node ^. prop InterpreterData . Interpreter.value of
        Left  err -> return $ Right $ PlainVal ("", [])
        Right val -> do
            val <- liftIO . runExceptT $ toExceptIO val
            case val of
                Left  s -> return $ Left s
                Right v -> caseTest (uncover tpNode) $ do
                    of' $ \(Cons (Lit.String n) as) -> case n of
                        "Stream"  -> return $ Right $ Listener $ \f -> attachListener (unsafeFromData v) (f . decoder)
                        "Twitter" -> return $ Right $ Listener $ \_ -> attachListener (unsafeFromData v) (const $ return ())
                        _         -> return $ Right $ PlainVal $ decoder v
                    of' $ \ANY -> return $ Right $ PlainVal ("", [])

readMeta :: NodeRef -> Command AST (Maybe NodeMeta)
readMeta ref = runASTOp $ HMap.lookup metaKey . view (prop Meta) <$> Builder.read ref

getError :: NodeRef -> Command AST (Maybe (APIError.Error TypeRep))
getError = runASTOp . getError'

getError' :: ASTOp m => NodeRef -> m (Maybe (APIError.Error TypeRep))
getError' ref = do
    n :: ASTNode <- Builder.read ref
    err <- mapM reprError $ n ^. prop TCData . tcErrors
    inps <- mapM (Builder.follow source) $ uncover n # Inputs
    inpErrs <- catMaybes <$> mapM getError' inps
    return $ tryHead err <|> tryHead inpErrs

reprError :: ASTOp m => TCError NodeRef -> m (APIError.Error TypeRep)
reprError tcErr = case tcErr of
    TCError.ImportError Nothing m  -> return $ APIError.ImportError m
    TCError.ImportError (Just n) m -> do
        tp <- Builder.follow source =<< Builder.follow (prop Type) n
        tpRep <- Printer.getTypeRep tp
        return $ APIError.NoMethodError m tpRep
    TCError.UnificationError uni -> do
        uniNode <- Builder.read uni
        caseTest (uncover uniNode) $ do
            of' $ \(Unify l r) -> APIError.TypeError <$> (Printer.getTypeRep =<< Builder.follow source l) <*> (Printer.getTypeRep =<< Builder.follow source r)
            of' $ \ANY         -> impossible

writeMeta :: NodeRef -> NodeMeta -> Command AST ()
writeMeta ref newMeta = runASTOp $ Builder.withRef ref $ prop Meta %~ HMap.insert metaKey newMeta

renameVar :: NodeRef -> String -> Command AST ()
renameVar = runASTOp .: ASTBuilder.renameVar

removeSubtree :: NodeRef -> Command AST ()
removeSubtree = runASTOp . safeRemove

printExpression :: NodeRef -> Command AST String
printExpression = runASTOp . Printer.printExpression

applyFunction :: NodeRef -> NodeRef -> Int -> Command AST NodeRef
applyFunction = runASTOp .:. ASTBuilder.applyFunction

redirectLambdaOutput :: NodeRef -> NodeRef -> Int -> Command AST NodeRef
redirectLambdaOutput lambdaRef newOutputRef pos = runASTOp $ do
    lambda <- Builder.read lambdaRef
    caseTest (uncover lambda) $ do
        of' $ \(Lam args _) -> do
            args' <- (mapM . mapM) (Builder.follow source) args
            Builder.lam args' newOutputRef
        of' $ \ANY -> throwError $ show lambdaRef ++ " is not lambda"

unapplyArgument :: NodeRef -> Int -> Command AST NodeRef
unapplyArgument = runASTOp .: ASTBuilder.removeArg

makeAccessor :: NodeRef -> NodeRef -> Command AST NodeRef
makeAccessor = runASTOp .: ASTBuilder.makeAccessor

removeAccessor :: NodeRef -> Command AST NodeRef
removeAccessor = runASTOp . ASTBuilder.unAcc

getTargetNode :: NodeRef -> Command AST NodeRef
getTargetNode nodeRef = runASTOp $ Builder.read nodeRef
                               >>= return . view ASTBuilder.rightMatchOperand
                               >>= Builder.follow source

getVarNode :: NodeRef -> Command AST NodeRef
getVarNode nodeRef = runASTOp $ Builder.read nodeRef
                            >>= return . view ASTBuilder.leftMatchOperand
                            >>= Builder.follow source

getLambdaInputRef :: NodeRef -> Int -> Command AST NodeRef
getLambdaInputRef nodeRef pos = runASTOp $ do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Lam args out) -> (!! pos) <$> ASTBuilder.unpackArguments args

isLambda :: NodeRef -> Command AST Bool
isLambda ref = runASTOp $ do
  node <- Builder.read ref
  caseTest (uncover node) $ do
    of' $ \(Lam _ _) -> return True
    of' $ \ANY       -> return False

isLambdaInput :: ASTOp m => NodeRef -> NodeRef -> m Bool
isLambdaInput node lambdaRef = do
    lambda <- Builder.read lambdaRef
    caseTest (uncover lambda) $ do
        of' $ \(Lam args _) -> (node `elem`) <$> ASTBuilder.unpackArguments args

getLambdaOutputRef :: NodeRef -> Command AST NodeRef
getLambdaOutputRef lambdaRef = runASTOp $ do
    lambda <- Builder.read lambdaRef
    caseTest (uncover lambda) $ do
        of' $ \(Lam _ out) -> Builder.follow source out

replaceTargetNode :: NodeRef -> NodeRef -> Command AST ()
replaceTargetNode matchNodeId newTargetId = runASTOp $ do
    Builder.reconnect ASTBuilder.rightMatchOperand matchNodeId newTargetId
    return ()

dumpGraphViz :: String -> Command AST ()
dumpGraphViz name = do
    g <- runASTOp Builder.get
    liftIO $ renderAndOpen [(name, name, g)]
