{-# LANGUAGE ExistentialQuantification #-}

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
import           Prologue (convert, convertVia, mempty, wrap)

import           Control.Monad.Catch          (catchAll)
import           Data.Char                    (digitToInt)
import qualified Data.List.Split              as Split
import qualified Data.Text                    as Text
import qualified Data.Scientific              as Scientific

import           Empire.ASTOp                    (EmpirePass, GraphOp)
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (ClsGraph, Graph)
import qualified Empire.Data.Graph               as Graph (codeMarkers)
import           Empire.Data.Layers              (SpanLength)
import qualified Empire.Commands.Code            as Code
import qualified Control.Monad.State.Layered as State

import           LunaStudio.Data.PortDefault     (PortDefault (..), PortValue (..))

import qualified Data.Text.Position              as Pos
import qualified Data.Mutable.Class              as Mutable
import qualified Data.Vector.Storable.Foreign    as Vector
import qualified Luna.IR.Layer as Layer
import qualified Luna.IR                         as IR
import qualified OCI.Pass.Definition.Declaration      as Pass
import qualified Luna.Pass                         as Pass
import qualified Luna.Pass.Scheduler as Scheduler
import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import qualified Luna.Syntax.Text.Parser.IR.Class as Token
import qualified Luna.Syntax.Text.Parser.Pass as Parser
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import qualified Luna.Syntax.Text.Parser.Data.Name.Special as Parser (uminus)
import qualified Data.Graph.Data.Graph.Class as LunaGraph
import           Luna.Pass.Data.Stage (Stage)

import qualified Empire.Pass.PatternTransformation            as PT
import qualified Luna.Pass.Attr as Attr
import qualified Luna.Syntax.Prettyprint as Prettyprint
import qualified Luna.Syntax.Text.Parser.IR.Term as Parsing
import Luna.Syntax.Text.Scope (Scope)
import qualified Luna.Syntax.Text.Source         as Source
import qualified Luna.IR.Term.Literal            as Lit
import Luna.Syntax.Text.Parser.Pass.Class (IRBS, Parser)
import Luna.Syntax.Text.Parser.Data.Result (Result (Result))

data SomeParserException = forall e. Exception e => SomeParserException e

deriving instance Show SomeParserException

instance Exception SomeParserException where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" <> displayException e <> ")"


parseExpr :: Text -> IO NodeRef
parseExpr s = view _1 <$> runParser Parsing.expr s `catchAll` (\e -> throwM $ SomeParserException e)

parsePattern :: Text -> IO NodeRef
parsePattern s = view _1 <$> runParser Parsing.pat s `catchAll` (\e -> throwM $ SomeParserException e)

passConverter :: (stage1 ~ stage2) => Pass.Pass stage1 pass1 a -> Pass.Pass stage2 pass2 a
passConverter = unsafeCoerce


runParser :: Token.Parser (IRBS IR.SomeTerm) -> Text -> IO (NodeRef, LunaGraph.State Stage, Scheduler.State, MarkedExprMap)
runParser parser input = do
    (((ir, m), scState), grState) <- LunaGraph.encodeAndEval @Stage $ do
        foo <- Scheduler.runT $ do
            ref <- liftIO $ newIORef (error "emptyreturn")
            Scheduler.registerPassFromFunction__ @Stage @EmpirePass $ do
                ((ir,scope), m) <- passConverter $ flip Parser.runParser__ (convert input) $ do
                    irb   <- parser
                    scope <- State.get @Scope
                    let Parser.IRBS irx = irb
                        irb' = Parser.IRBS irx
                    pure $ (,scope) <$> irb'
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
runProperParser code = runParser Parsing.unit' code `catchAll` (\e -> throwM $ SomeParserException e) -- do

runProperVarParser :: Text.Text -> IO ()
runProperVarParser code = (void $ runParser Parsing.var code) `catchAll` (\e -> throwM $ SomeParserException e)

runProperPatternParser :: Text.Text -> IO NodeRef
runProperPatternParser code = parsePattern code

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
    return (view _1 parse, input)

runFunParser :: Text.Text -> Command ClsGraph (NodeRef, LunaGraph.State Stage, Scheduler.State, MarkedExprMap)
runFunParser expr = liftIO $
    runParser (Parsing.possiblyDocumented Parsing.func) expr
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



parsePortDefault :: PortDefault -> GraphOp NodeRef
parsePortDefault (Expression expr)          = do
    ref <- liftIO $ parseExpr (convert expr)
    Code.propagateLengths ref
    return ref
parsePortDefault (Constant (IntValue  i))
    | i >= 0     = do
        intPart <- Mutable.fromList $ map (fromIntegral . digitToInt) $ show i
        empty   <- Mutable.new
        generalize <$> IR.number 10 intPart empty `withLength` (length $ show i)
    | otherwise = do
        intPart <- Mutable.fromList $ map (fromIntegral . digitToInt) $ show (abs i)
        empty   <- Mutable.new
        number <- generalize <$> IR.number 10 intPart empty `withLength` (length $ show $ abs i)
        minus  <- generalize <$> IR.var Parser.uminus `withLength` 1
        app    <- generalize <$> IR.app minus number `withLength` (1 + length (show (abs i)))
        return app
parsePortDefault (Constant (TextValue s)) = do
    l <- Mutable.fromList s
    generalize <$> IR.rawString l `withLength` (length s)
parsePortDefault (Constant (RealValue d)) = do
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
                minus <- generalize <$> IR.var Parser.uminus `withLength` minusLength
                app   <- generalize <$> IR.app minus number `withLength`
                    (minusLength + intLength + dotLength + fracLength)
                return app
            else
                return number
        _ -> throwM $ PortDefaultNotConstructibleException (Constant (RealValue d))
parsePortDefault (Constant (BoolValue b)) = generalize <$> IR.cons (convert $ show b) []  `withLength` (length $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
