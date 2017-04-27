module Empire.Commands.Lexer (lexer) where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import Empire.Prelude

import           Data.SpanTree                       (RightSpacedSpan(..))
import           Data.Text.Position                  (Delta(..))
import           Luna.Syntax.Text.Parser.Lexer.Class (Token(..))
import qualified Luna.Syntax.Text.Parser.Lexer       as Lexer



lexer :: Text -> [(Int, [String])]
lexer code = explodedTokens
    where
        Lexer.Stream tokens = Lexer.runLexer $ Text.unpack code
        tokensWithTags      = map (\a -> (a, Lexer.tags a)) tokens
        explodedTokens      = concatMap explodeToken tokensWithTags

explodeToken :: (Token Lexer.Symbol, Lexer.Tags) -> [(Int, [String])]
explodeToken (Token (RightSpacedSpan len (Delta 0)) _, tags) = [(fromIntegral len, stringify tags)]
explodeToken (Token (RightSpacedSpan len off) _, tags) = [(fromIntegral len, stringify tags), (fromIntegral off, [])]

stringify :: Lexer.Tags -> [String]
stringify tags = Set.toList $ Set.map nameToString tags
