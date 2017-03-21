{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Parse (
    parseExpr
  , parsePortDefault
  , runParser
  ) where

import           Data.Convert
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, unwrap', mempty)

import           Data.Char                    (isUpper)
import           Data.List                    (partition)
import qualified Data.Text                    as Text

import           Empire.ASTOp                    (ASTOp, runPass)
import           Empire.ASTOps.Builder           (lams)
import           Empire.ASTOps.Print
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (Graph)
import           Empire.Data.Parser              (ParserPass)

import           Empire.API.Data.PortDefault     (PortDefault (..), Value (..))

import           Data.TypeDesc                   (getTypeDesc)
import qualified Luna.IR                         as IR
import qualified Luna.Syntax.Text.Parser.Marker  as Parser (MarkedExprMap)
import qualified Luna.Syntax.Text.Parser.Parser  as Parser
import qualified Luna.Syntax.Text.Parser.Parsing as Parsing
import qualified Luna.Syntax.Text.Source         as Source

data ParserException e = ParserException e
    deriving (Show)

instance Exception e => Exception (ParserException e) where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException (ParserException e) = "ParserException (" ++ displayException e ++ ")"

parseExpr :: ASTOp m => String -> m NodeRef
parseExpr s = do
    IR.writeAttr @Source.Source $ convert s
    Parsing.parsingBase Parsing.expr
    res     <- IR.readAttr @Parser.ParsedExpr
    exprMap <- IR.readAttr @Parser.MarkedExprMap
    return $ unwrap' res

runParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runParser expr = do
    let inits = do
            IR.setAttr (getTypeDesc @Source.SourceTree)    $ (mempty :: Source.SourceTree)
            IR.setAttr (getTypeDesc @Parser.MarkedExprMap) $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)        $ (error "Data not provided: Source")
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)    $ (error "Data not provided: ParsedExpr")
        run = runPass @ParserPass inits
    run $ do
        IR.writeAttr @Source.Source $ convert expr
        Parsing.parsingBase Parsing.expr
        res     <- IR.readAttr @Parser.ParsedExpr
        exprMap <- IR.readAttr @Parser.MarkedExprMap
        return (unwrap' res, exprMap)

data PortDefaultNotConstructibleException = PortDefaultNotConstructibleException PortDefault
    deriving Show

instance Exception PortDefaultNotConstructibleException where
    toException = astExceptionToException
    fromException = astExceptionFromException

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)            = parseExpr expr
parsePortDefault (Constant (IntValue i))      = IR.generalize <$> IR.number (fromIntegral i)
parsePortDefault (Constant (StringValue s))   = IR.generalize <$> IR.string s
parsePortDefault (Constant (DoubleValue d))   = $notImplemented
parsePortDefault (Constant (RationalValue r)) = $notImplemented
parsePortDefault (Constant (BoolValue b))     = IR.generalize <$> IR.cons_ (convert $ show b)
parsePortDefault d = throwM $ PortDefaultNotConstructibleException d
