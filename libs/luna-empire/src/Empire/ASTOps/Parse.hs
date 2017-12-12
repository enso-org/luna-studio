{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Empire.ASTOps.Parse (
    SomeParserException
  , FunctionParsing(..)
  , parseExpr
  , parsePattern
  , parsePortDefault
  , runParser
  , runFunHackParser
  , runReparser
  , runProperParser
  , runProperVarParser
  , runProperPatternParser
  ) where

import           Data.Convert
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, convertVia, unwrap', mempty, wrap', wrap)

import           Control.Monad.Catch          (catchAll)
import qualified Data.Text                    as Text

import           Empire.ASTOp                    (GraphOp, PMStack, runPass, runPM)
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (ClsGraph, Graph)
import qualified Empire.Data.Graph               as Graph (codeMarkers)
import           Empire.Data.Layers              (attachEmpireLayers, SpanLength)
import           Empire.Data.Parser              (ParserPass)
import qualified Empire.Commands.Code            as Code

import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))

import qualified Data.Text.Position              as Pos
import           Data.TypeDesc                   (getTypeDesc)
import qualified Luna.Builtin.Data.Function      as Function (compile)
import qualified Luna.IR                         as IR
import qualified Luna.Syntax.Text.Layer.Loc      as Loc
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Errors  (Invalids)
import qualified Luna.Syntax.Text.Parser.Hardcoded as Parser (uminusName)
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
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" <> displayException e <> ")"

parseExpr :: GraphOp m => String -> m NodeRef
parseExpr s = do
    IR.putAttr @Source.Source $ convert s
    Parsing.parsingPassM Parsing.expr
    res     <- IR.getAttr @Parser.ParsedExpr
    exprMap <- IR.getAttr @Parser.MarkedExprMap
    return $ unwrap' res

parsePattern :: GraphOp m => Text.Text -> m NodeRef
parsePattern s = do
    IR.putAttr @Source.Source $ convert s
    Parsing.parsingPassM Parsing.pattern
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
    IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
    IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
    IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
    IR.setAttr (getTypeDesc @Invalids) $ (mempty :: Invalids)

instance Convertible Text Source.Source where
    convert t = Source.Source (convertVia @String t)

runProperParser :: Text.Text -> IO (NodeRef, IR.Rooted NodeRef, Parser.MarkedExprMap)
runProperParser code = do
    runPM $ do
        parserBoilerplate
        attachEmpireLayers
        IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
        (unit, root) <- Pass.eval' @ParserPass $ do
            Parsing.parsingPassM Parsing.unit' `catchAll` (\e -> throwM $ SomeParserException e)
            res  <- IR.getAttr @Parser.ParsedExpr
            root <- Function.compile (unwrap' res)
            return (unwrap' res, root)
        Just exprMap <- unsafeCoerce <$> IR.unsafeGetAttr (getTypeDesc @Parser.MarkedExprMap)
        return (unit, root, exprMap)

runProperVarParser :: Text.Text -> IO NodeRef
runProperVarParser code = do
    runPM $ do
        parserBoilerplate
        attachEmpireLayers
        IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
        var <- Pass.eval' @ParserPass $ do
            Parsing.parsingPassM Parsing.var `catchAll` (\e -> throwM $ SomeParserException e)
            res  <- IR.getAttr @Parser.ParsedExpr
            return (unwrap' res)
        return var

runProperPatternParser :: Text.Text -> IO NodeRef
runProperPatternParser code = do
    runPM $ do
        parserBoilerplate
        attachEmpireLayers
        IR.setAttr (getTypeDesc @Source.Source) $ (convert code :: Source.Source)
        pattern <- Pass.eval' @ParserPass $ do
            Parsing.parsingPassM Parsing.pattern `catchAll` (\e -> throwM $ SomeParserException e)
            res  <- IR.getAttr @Parser.ParsedExpr
            return (unwrap' res)
        return pattern

runParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runParser expr = do
    let inits = do
            IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @Graph @ParserPass inits
    run $ do
        Parsing.parsingPassM Parsing.expr `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        return (unwrap' res, exprMap)

prepareInput :: Text.Text -> FunctionParsing -> Text.Text
prepareInput expr parsing = Text.concat $ header : case parsing of
    AppendNone -> [":\n    None"]
    ParseAsIs  -> []
    where
        stripped = Text.strip expr
        header   = case Text.splitOn " " stripped of
            (def:var:args) -> Text.intercalate " " (def:var:args)
            i              -> Text.concat i


data FunctionParsing = AppendNone | ParseAsIs

runFunHackParser :: Text.Text -> FunctionParsing -> Command ClsGraph (NodeRef, Text.Text)
runFunHackParser expr parsing = do
    let input = prepareInput expr parsing
    parse <- runFunParser input
    return (fst parse, input)

runFunParser :: Text.Text -> Command ClsGraph (NodeRef, Parser.MarkedExprMap)
runFunParser expr = do
    let inits = do
            IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ClsGraph @ParserPass inits
    run $ do
        Parsing.parsingPassM (Parsing.possiblyDocumented $ (Parsing.rootedRawFunc <|> Parsing.func)) `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        return (unwrap' res, exprMap)

runReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, Parser.MarkedExprMap, Parser.ReparsingStatus)
runReparser expr oldExpr = do
    let inits = do
            IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @Graph @ParserPass inits
    run $ do
        do
            gidMapOld <- use Graph.codeMarkers

            -- parsing new file and updating updated analysis
            Parsing.parsingPassM Parsing.valExpr `catchAll` (\e -> throwM $ SomeParserException e)
            gidMap    <- IR.getAttr @Parser.MarkedExprMap

            -- Preparing reparsing status
            rs        <- Parsing.cmpMarkedExprMaps (wrap' gidMapOld) gidMap
            IR.putAttr @Parser.ReparsingStatus (wrap rs)

        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        status  <- IR.getAttr @Parser.ReparsingStatus
        return (unwrap' res, exprMap, status)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

infixr 0 `withLength`
withLength :: GraphOp m => m NodeRef -> Int -> m NodeRef
withLength act len = do
    ref <- act
    IR.putLayer @SpanLength ref (convert len)
    return ref

parsePortDefault :: GraphOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = do
    ref <- parseExpr expr
    Code.propagateLengths ref
    return ref
parsePortDefault (Constant (IntValue  i))
    | i >= 0     = IR.generalize <$> IR.number (fromIntegral i) `withLength` (length $ show i)
    | otherwise = do
        number <- IR.generalize <$> IR.number (fromIntegral (abs i)) `withLength` (length $ show $ abs i)
        minus  <- IR.generalize <$> IR.var Parser.uminusName `withLength` 1
        app    <- IR.generalize <$> IR.app minus number `withLength` (1 + length (show (abs i)))
        return app
parsePortDefault (Constant (TextValue s)) = IR.generalize <$> IR.string s                  `withLength` (length s)
parsePortDefault (Constant (RealValue d)) = IR.generalize <$> IR.number (Lit.fromDouble d) `withLength` (length $ show d)
parsePortDefault (Constant (BoolValue b)) = IR.generalize <$> IR.cons_ (convert $ show b)  `withLength` (length $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
