{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures     #-}

module Empire.ASTOps.Parse (
    SomeParserException
  , FunctionParsing(..)
  , parseExpr
  , parsePattern
  , parsePortDefault
  , runParser
  , runFunHackParser
  , runProperParser
  , runProperVarParser
  , runProperPatternParser
  ) where

import           Data.Convert
import           Data.IORef
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, convertVia)

import           Data.Char                    (digitToInt)
import qualified Data.List.Split              as Split
import qualified Data.Text                    as Text
import qualified Data.Scientific              as Scientific

import           Empire.ASTOp                    (EmpirePass, GraphOp, liftScheduler, runASTOp)
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (Graph)
import           Empire.Data.Layers              (SpanLength)
import qualified Empire.Commands.Code            as Code
import qualified Control.Monad.State.Layered as State

import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))

import qualified Data.Mutable.Class              as Mutable
import qualified Luna.IR                         as IR
import qualified Luna.Pass                         as Pass
import qualified Luna.Pass.Scheduler as Scheduler
import qualified Luna.Syntax.Text.Parser.Lexer.Names   as Parser (uminus)
import qualified Data.Graph.Data.Graph.Class as LunaGraph
import           Luna.Pass.Data.Stage (Stage)

import qualified Luna.Syntax.Text.Parser.Ast           as Parsing
import qualified Luna.Syntax.Text.Source         as Source
import qualified Luna.Syntax.Text.Parser.State.Marker      as Marker
import qualified Luna.Syntax.Text.Parser.Lexer         as Lexer

import qualified Luna.Syntax.Text.Parser.Parser.Class                     as Macro
import qualified Luna.Syntax.Text.Parser.Hardcoded as Hardcoded
import qualified Luna.Syntax.Text.Parser.State.Version as Syntax

import qualified Luna.Pass.Parsing.Parser as Parser3

data SomeParserException = forall e. Exception e => SomeParserException e

deriving instance Show SomeParserException

instance Exception SomeParserException where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" <> displayException e <> ")"

parsePattern :: Text -> Command g NodeRef
parsePattern s = view _1 <$> parse3 Macro.expr s `catchAll` (\e -> throwM $ SomeParserException e)

passConverter :: (stage1 ~ stage2) => Pass.Pass stage1 pass1 a -> Pass.Pass stage2 pass2 a
passConverter = unsafeCoerce

parseExpr :: Text -> Command g NodeRef
parseExpr s = view _1 <$> parse3 Macro.expr s

parse3 :: Macro.Parser (Parsing.Spanned Parsing.Ast) -> Text -> Command g (NodeRef, MarkedExprMap)
parse3 parser input = do
    (ir, m) <- do
        ref <- liftIO $ newIORef (error "emptyreturn")
        foo <- liftScheduler $ do
            Scheduler.registerPassFromFunction__ @Stage @EmpirePass $ do
                (ir, m) <- passConverter $ run parser (convert input)
                liftIO $ writeIORef ref $ generalize (ir, m)
            Scheduler.runPassByType @EmpirePass
            liftIO $ readIORef ref
        return foo
    return (ir, m)

runWith :: Macro.Parser (Parsing.Spanned Parsing.Ast) -> Lexer.Source -> (Parsing.Spanned Parsing.Ast)
runWith = \p src -> let
    toks = Lexer.eval Syntax.Version1 src
    foo = Macro.evalStack toks $ do
        Hardcoded.hardcode
        Macro.hardcodePredefinedMacros
        p
    in case foo of
        Left err -> trace err undefined
        Right a -> a
{-# NOINLINE runWith #-}

run :: Parser3.ParserPass (Pass.Pass stage Parser3.Parser)
    => Macro.Parser (Parsing.Spanned Parsing.Ast) -> Lexer.Source -> Pass.Pass stage Parser3.Parser (IR.SomeTerm, Marker.TermMap)
run parser src = do
    ((ref, _unmarked), gidMap) <- State.runDefT @Marker.TermMap
                                $ State.runDefT @Marker.TermOrphanList
                                $ Parser3.buildIR $ runWith parser src
    pure (ref, gidMap)

runParser :: Macro.Parser (Parsing.Spanned Parsing.Ast) -> Text -> IO (NodeRef, LunaGraph.State Stage, Scheduler.State, MarkedExprMap)
runParser parser input = do
    (((ir, m), scState), grState) <- LunaGraph.encodeAndEval @Stage $ do
        foo <- Scheduler.runT $ do
            ref <- liftIO $ newIORef (error "emptyreturn")
            Scheduler.registerPassFromFunction__ @Stage @EmpirePass $ do
                (ir, m) <- passConverter $ run parser (convert input)
                liftIO $ writeIORef ref $ generalize (ir, m)
            Scheduler.runPassByType @EmpirePass
            foo <- liftIO $ readIORef ref
            return foo
        st <- LunaGraph.getState
        return (foo, st)
    return (ir, grState, scState, m)

instance Convertible Text Source.Source where
    convert t = Source.Source (convertVia @String t)

runProperParser :: Text.Text -> IO (NodeRef, LunaGraph.State Stage, Scheduler.State, MarkedExprMap)
runProperParser code = runParser Macro.unit code `catchAll` (\e -> throwM $ SomeParserException e) -- do

runProperVarParser :: Text.Text -> IO ()
runProperVarParser code = do
    (case Lexer.isSingleVar (Lexer.evalVersion1 (convert code)) of
        True -> return ()
        _    -> error ("incorrect var " <> convert code)) `catchAll` (\e -> throwM $ SomeParserException e)

runProperPatternParser :: MonadCatch m => Text.Text -> m ()
runProperPatternParser code = do
    (case Lexer.isCorrectPattern (Lexer.evalVersion1With Lexer.exprs (convert code)) of
        True -> return ()
        _    -> error ("incorrect pattern " <> convert code)) `catchAll` (\e -> throwM $ SomeParserException e)

data FunctionParsing = AppendNone | ParseAsIs

runFunHackParser :: Text.Text -> FunctionParsing -> Command g (NodeRef, Text.Text)
runFunHackParser expr _ = do
    parse <- runFunParser expr
    return (view _1 parse, expr)

runFunParser :: Text.Text -> Command g (NodeRef, MarkedExprMap)
runFunParser expr = parse3 (Macro.expr) expr
        `catchAll` (\e -> throwM $ SomeParserException e)

-- runReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, MarkedExprMap, Parser.ReparsingStatus)
-- runReparser expr oldExpr = do
--     let inits = do
--             return ()
--             -- IR.setAttr (getTypeDesc @Invalids)               $ (mempty :: Invalids)

--             -- IR.setAttr (getTypeDesc @MarkedExprMap)   $ (mempty :: MarkedExprMap)
--             -- IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
--             -- IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
--             -- IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
--         run = runPass @Graph @ParserPass inits
--     run $ do
--         do
--             gidMapOld <- use Graph.codeMarkers

--             -- parsing new file and updating updated analysis
--             Parsing.parsingPassM Parsing.valExpr `catchAll` (\e -> throwM $ SomeParserException e)
--             gidMap    <- getAttr @MarkedExprMap

--             -- Preparing reparsing status
--             rs        <- Parsing.cmpMarkedExprMaps (wrap' gidMapOld) gidMap
--             putAttr @Parser.ReparsingStatus (wrap rs)

--         res     <- getAttr @Parser.ParsedExpr
--         exprMap <- getAttr @MarkedExprMap
--         status  <- getAttr @Parser.ReparsingStatus
--         return (unwrap' res, exprMap, status)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

infixr 0 `withLength`
withLength :: GraphOp NodeRef -> Int -> GraphOp NodeRef
withLength act len = do
    ref <- act
    putLayer @SpanLength ref (convert len)
    return ref



parsePortDefault :: PortDefault -> Command Graph NodeRef
parsePortDefault (Expression expr) = do
    ref <- parseExpr (convert expr)
    runASTOp $ Code.propagateLengths ref
    return ref
parsePortDefault (Constant (IntValue  i))
    | i >= 0     = runASTOp $ do
        intPart <- Mutable.fromList $ map (fromIntegral . digitToInt) $ show i
        empty'  <- Mutable.new
        generalize <$> IR.number 10 intPart empty' `withLength` (length $ show i)
    | otherwise = runASTOp $ do
        intPart <- Mutable.fromList $ map (fromIntegral . digitToInt) $ show (abs i)
        empty'  <- Mutable.new
        number  <- generalize <$>
            IR.number 10 intPart empty' `withLength` (length $ show $ abs i)
        minus   <- generalize <$>
            IR.var Parser.uminus `withLength` 1
        app     <- generalize <$>
            IR.app minus number `withLength` (1 + length (show (abs i)))
        return app
parsePortDefault (Constant (TextValue s)) = runASTOp $ do
    l <- Mutable.fromList s
    generalize <$> IR.rawString l `withLength` (length s)
parsePortDefault (Constant (RealValue d)) = runASTOp $ do
    let negative    = d < 0
        sc          = Scientific.fromFloatDigits $ abs d
        scString    = Scientific.formatScientific Scientific.Fixed Nothing sc
    case Split.splitOn "." scString of
        [int, frac] -> do
            intPart <- Mutable.fromList $ map (fromIntegral . digitToInt) int
            let minusLength = 1
                dotLength   = 1
                intLength   = length int
                fracLength  = length frac
            fracPart <- Mutable.fromList $ map (fromIntegral . digitToInt) frac
            number   <- generalize <$> IR.number 10 intPart fracPart `withLength`
                (intLength + dotLength + fracLength)
            if negative then do
                minus <- generalize <$>
                    IR.var Parser.uminus `withLength` minusLength
                app   <- generalize <$> IR.app minus number `withLength`
                    (minusLength + intLength + dotLength + fracLength)
                return app
            else
                return number
        _ -> throwM $ PortDefaultNotConstructibleException (Constant (RealValue d))
parsePortDefault (Constant (BoolValue b)) = runASTOp $ generalize <$>
    IR.cons (convert $ show b) [] `withLength` length (show b)
