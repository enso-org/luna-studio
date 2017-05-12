{-# LANGUAGE ExistentialQuantification #-}
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
  ) where

import           Data.Convert
import           Empire.Empire
import           Empire.Prelude hiding (mempty)
import           Prologue (convert, unwrap', mempty, wrap', wrap)

import           Control.Monad.Catch          (catchAll)
import           Data.Char                    (isUpper)
import           Data.List                    (partition)
import qualified Data.Text                    as Text

import           Empire.ASTOp                    (ASTOp, runPass)
import           Empire.ASTOps.Builder           (lams)
import           Empire.ASTOps.Print
import           Empire.Data.AST                 (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph               (Graph)
import           Empire.Data.Layers              (CodeMarkers)
import           Empire.Data.Parser              (ParserPass)

import           Empire.API.Data.PortDefault     (PortDefault (..), PortValue (..))

import           Data.TypeDesc                   (getTypeDesc)
import qualified Luna.IR                         as IR
import qualified Luna.Syntax.Text.Parser.Marker  as Parser (MarkedExprMap)
import qualified Luna.Syntax.Text.Parser.Parser  as Parser
import qualified Luna.Syntax.Text.Parser.Parsing as Parsing
import qualified Luna.Syntax.Text.Source         as Source
import qualified Luna.IR.Term.Literal            as Lit

data SomeParserException = forall e. Exception e => SomeParserException e

deriving instance Show SomeParserException

instance Exception SomeParserException where
    toException = astExceptionToException
    fromException = astExceptionFromException
    displayException exc = case exc of SomeParserException e -> "SomeParserException (" ++ displayException e ++ ")"

parseExpr :: ASTOp m => String -> m NodeRef
parseExpr s = do
    IR.putAttr @Source.Source $ convert s
    Parsing.parsingBase Parsing.expr
    res     <- IR.getAttr @Parser.ParsedExpr
    exprMap <- IR.getAttr @Parser.MarkedExprMap
    return $ unwrap' res

runUnitParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runUnitParser code = do
    let inits = do
          IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
          IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
          IR.setAttr (getTypeDesc @Source.Source)          $ (convert code :: Source.Source)
          IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
          IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        let protoFunc = (\body -> uncurry Parsing.seqLines body)
        Parsing.parsingBase (protoFunc <$> Parsing.discoverBlock Parsing.lineExpr) `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        IR.putLayer @CodeMarkers (unwrap' res) exprMap
        return (unwrap' res, exprMap)

runUnitReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, Parser.MarkedExprMap, Parser.ReparsingStatus)
runUnitReparser code oldExpr = do
    let inits = do
          IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
          IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
          IR.setAttr (getTypeDesc @Source.Source)          $ (convert code :: Source.Source)
          IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
          IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        do
            gidMapOld <- IR.getLayer @CodeMarkers oldExpr

            -- parsing new file and updating updated analysis
            let protoFunc = (\body -> uncurry Parsing.seqLines body)
            Parsing.parsingBase (protoFunc <$> Parsing.discoverBlock Parsing.lineExpr) `catchAll` (\e -> throwM $ SomeParserException e)
            gidMap    <- IR.getAttr @Parser.MarkedExprMap

            -- Preparing reparsing status
            rs        <- Parsing.cmpMarkedExprMaps gidMapOld gidMap
            IR.putAttr @Parser.ReparsingStatus (wrap rs)

        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        rs      <- IR.getAttr @Parser.ReparsingStatus
        IR.putLayer @CodeMarkers (unwrap' res) exprMap
        return (unwrap' res, exprMap, rs)

runParser :: Text.Text -> Command Graph (NodeRef, Parser.MarkedExprMap)
runParser expr = do
    let inits = do
            IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (error "Data not provided: ParsedExpr")
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        Parsing.parsingBase Parsing.expr `catchAll` (\e -> throwM $ SomeParserException e)
        res     <- IR.getAttr @Parser.ParsedExpr
        exprMap <- IR.getAttr @Parser.MarkedExprMap
        IR.putLayer @CodeMarkers (unwrap' res) exprMap
        return (unwrap' res, exprMap)

runReparser :: Text.Text -> NodeRef -> Command Graph (NodeRef, Parser.MarkedExprMap, Parser.ReparsingStatus)
runReparser expr oldExpr = do
    let inits = do
            IR.setAttr (getTypeDesc @Source.SourceTree)      $ (mempty :: Source.SourceTree)
            IR.setAttr (getTypeDesc @Parser.MarkedExprMap)   $ (mempty :: Parser.MarkedExprMap)
            IR.setAttr (getTypeDesc @Source.Source)          $ (convert expr :: Source.Source)
            IR.setAttr (getTypeDesc @Parser.ParsedExpr)      $ (wrap' oldExpr :: Parser.ParsedExpr)
            IR.setAttr (getTypeDesc @Parser.ReparsingStatus) $ (error "Data not provided: ReparsingStatus")
        run = runPass @ParserPass inits
    run $ do
        do
            gidMapOld <- IR.getLayer @CodeMarkers oldExpr

            -- parsing new file and updating updated analysis
            Parsing.parsingBase Parsing.nonAssignmentExpr `catchAll` (\e -> throwM $ SomeParserException e)
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
