{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Empire.ASTOps.Parse (
    SomeParserException
  , parseExpr
  , parsePortDefault
  , runParser
  , runReparser
  , runUnitParser
  , runUnitReparser
  , runProperParser
  ) where

import           Data.Convert
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, unwrap', mempty, wrap', wrap)

import           Control.Monad.Catch          (catchAll)
import           Data.Char                    (isUpper)
import           Data.List                    (partition)
import qualified Data.Map                     as Map
import qualified Data.Text                    as Text

import           Empire.ASTOp                    (ASTOp, PMStack, runPass, runPM)
import           Empire.ASTOps.Builder           (lams)
import           Empire.ASTOps.Print
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (Graph)
import           Empire.Data.Layers              (CodeMarkers, attachEmpireLayers)
import           Empire.Data.Parser              (ParserPass)

import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))

import qualified Data.Text.Position              as Pos
import           Data.TypeDesc                   (getTypeDesc)
import qualified Luna.Builtin.Data.Function      as Function (importRooted)
import qualified Luna.IR                         as IR
import qualified Luna.Syntax.Text.Layer.Loc      as Loc
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Errors  (Invalids)
import qualified Luna.Syntax.Text.Parser.Marker  as Parser (MarkedExprMap(..))
import qualified Luna.Syntax.Text.Parser.Parser  as Parser
import qualified Luna.Syntax.Text.Parser.Parsing as Parsing
import qualified Luna.Syntax.Text.Source         as Source
import qualified Luna.IR.Term.Literal            as Lit
import qualified OCI.Pass                        as Pass

data SomeParserException = forall e. Exception e => SomeParserException e

deriving instance Show SomeParserException

instance Exception SomeParserException where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" ++ displayException e ++ ")"

parseExpr :: ASTOp m => String -> m NodeRef
parseExpr s = do
    IR.putAttr @Source.Source $ convert s
    Parsing.parsingPassM Parsing.expr
    res     <- IR.getAttr @Parser.ParsedExpr
    exprMap <- IR.getAttr @Parser.MarkedExprMap
    return $ unwrap' res

parserBoilerplate :: PMStack IO ()
parserBoilerplate = do
    IR.runRegs
    Loc.init
    IR.attachLayer 5 (getTypeDesc @Pos.Range)         (getTypeDesc @IR.AnyExpr)
    CodeSpan.init
    IR.attachLayer 5 (getTypeDesc @CodeSpan.CodeSpan) (getTypeDesc @IR.AnyExpr)
    IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
    IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
    IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
    IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
    IR.setAttr (getTypeDesc @Invalids) $ (mempty :: Invalids)

runProperParser :: Text.Text -> IO (NodeRef, IR.Rooted NodeRef, Parser.MarkedExprMap)
runProperParser code = do
    runPM $ do
        parserBoilerplate
        attachEmpireLayers
        IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
        (unit, root) <- Pass.eval' @Parser.Reparsing $ do
            Parsing.parsingPassM Parsing.unit' `catchAll` (\e -> throwM $ SomeParserException e)
            res                 <- IR.getAttr @Parser.ParsedExpr
            root <- IR.matchExpr (unwrap' res) $ \case
                IR.Unit _ _ cls -> do
                    klass <- IR.source cls
                    IR.matchExpr klass $ \case
                        IR.ClsASG _ _ _ (main : _) -> do
                            main' <- IR.source main
                            IR.matchExpr main' $ \case
                                IR.ASGFunction _ rootedIR -> return rootedIR
            return (unwrap' res, root)
        Just exprMap <- unsafeCoerce <$> IR.unsafeGetAttr (getTypeDesc @Parser.MarkedExprMap)
        return (unit, root, exprMap)

runUnitParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runUnitParser code = do
    let inits = do
          IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
          IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)
          IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
          IR.setAttr (getTypeDesc @Source.Source)          $ (convert code :: Source.Source)
          IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
          IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        Parsing.parsingPassM Parsing.unit' `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        funRoot@(IR.Rooted _ funExpr) <- IR.matchExpr (unwrap' res) $ \case
            IR.Unit _ _ cls -> do
                klass <- IR.source cls
                IR.matchExpr klass $ \case
                    IR.ClsASG _ _ _ (main : _) -> do
                        main' <- IR.source main
                        IR.matchExpr main' $ \case
                            IR.ASGFunction _ rootedIR -> return rootedIR
        translator <- Function.importRooted funRoot
        exprMap    <- unwrap' <$> IR.getAttr @Parser.MarkedExprMap
        let transMap = Parser.MarkedExprMap $ Map.map translator exprMap
        IR.putLayer @CodeMarkers (translator funExpr) transMap
        return (translator funExpr, transMap)

runUnitReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, Parser.MarkedExprMap, Parser.ReparsingStatus)
runUnitReparser code oldExpr = do
    let inits = do
          IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
          IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

          IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
          IR.setAttr (getTypeDesc @Source.Source)          $ (convert code :: Source.Source)
          IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
          IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        (expr, exprMap) <- do
            gidMapOld <- IR.getLayer @CodeMarkers oldExpr

            -- parsing new file and updating updated analysis
            Parsing.parsingPassM Parsing.unit' `catchAll` (\e -> throwM $ SomeParserException e)
            res       <- IR.getAttr @Parser.ParsedExpr
            funRoot@(IR.Rooted _ funExpr) <- IR.matchExpr (unwrap' res) $ \case
                IR.Unit _ _ cls -> do
                    klass <- IR.source cls
                    IR.matchExpr klass $ \case
                        IR.ClsASG _ _ _ (main : _) -> do
                            main' <- IR.source main
                            IR.matchExpr main' $ \case
                                IR.ASGFunction _ rootedIR -> return rootedIR
            translator <- Function.importRooted funRoot
            exprMap    <- unwrap' <$> IR.getAttr @Parser.MarkedExprMap
            let transMap = Parser.MarkedExprMap $ Map.map translator exprMap

            -- Preparing reparsing status
            rs        <- Parsing.cmpMarkedExprMaps gidMapOld transMap
            IR.putAttr @Parser.ReparsingStatus (wrap rs)
            return (translator funExpr, transMap)

        exprMap <- IR.getAttr @Parser.MarkedExprMap
        rs      <- IR.getAttr @Parser.ReparsingStatus
        IR.putLayer @CodeMarkers (expr) exprMap
        return (expr, exprMap, rs)

runParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runParser expr = do
    let inits = do
            IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
            IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        Parsing.parsingPassM Parsing.expr `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        IR.putLayer @CodeMarkers (unwrap' res) exprMap
        return (unwrap' res, exprMap)

runReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, Parser.MarkedExprMap, Parser.ReparsingStatus)
runReparser expr oldExpr = do
    let inits = do
            IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
            IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        do
            gidMapOld <- IR.getLayer @CodeMarkers oldExpr

            -- parsing new file and updating updated analysis
            Parsing.parsingPassM Parsing.valExpr2 `catchAll` (\e -> throwM $ SomeParserException e)
            gidMap    <- IR.getAttr @Parser.MarkedExprMap

            -- Preparing reparsing status
            rs        <- Parsing.cmpMarkedExprMaps gidMapOld gidMap
            IR.putAttr @Parser.ReparsingStatus (wrap rs)

        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        IR.putLayer @CodeMarkers (unwrap' res) exprMap
        status  <- IR.getAttr @Parser.ReparsingStatus
        return (unwrap' res, exprMap, status)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = parseExpr expr
parsePortDefault (Constant (IntValue    i)) = IR.generalize <$> IR.number (fromIntegral i)
parsePortDefault (Constant (StringValue s)) = IR.generalize <$> IR.string s
parsePortDefault (Constant (DoubleValue d)) = IR.generalize <$> IR.number (Lit.fromDouble d)
parsePortDefault (Constant (BoolValue   b)) = IR.generalize <$> IR.cons_ (convert $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
