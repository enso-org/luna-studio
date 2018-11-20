module NodeEditor.React.Model.Searcher.Input where

import Common.Prelude

import qualified Data.Text              as Text
import qualified Data.Vector.Unboxed    as Vector
import qualified Luna.Syntax.Text.Lexer as Lexer

import Data.Convert (Convertible (convert))
import Data.Text32  (Text32)


data Divided = Divided
    { _prefix :: Text
    , _query  :: Text
    , _suffix :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Divided

instance Default Divided where def = Divided mempty mempty mempty

data Input
    = RawInput     Text
    | DividedInput Divided
    deriving (Eq, Generic, Show)

makePrisms ''Input

instance Default Input where def = RawInput def

instance Convertible Input Text where
    convert (RawInput     t) = t
    convert (DividedInput d) = convert d

instance Convertible Divided Text where
    convert (Divided p q s) = p <> q <> s

fromStream :: Text -> [Lexer.Token Lexer.Symbol] -> Int -> Input
fromStream input' inputStream pos = getInput queryBegin pos where
    queryBegin = findQueryBegin inputStream pos
    isQuery :: Lexer.Symbol -> Bool
    isQuery (Lexer.Var      {}) = True
    isQuery (Lexer.Cons     {}) = True
    isQuery (Lexer.Wildcard {}) = True
    isQuery (Lexer.KwAll    {}) = True
    isQuery (Lexer.KwCase   {}) = True
    isQuery (Lexer.KwClass  {}) = True
    isQuery (Lexer.KwDef    {}) = True
    isQuery (Lexer.KwImport {}) = True
    isQuery (Lexer.KwOf     {}) = True
    isQuery (Lexer.Operator {}) = True
    isQuery (Lexer.Modifier {}) = True
    isQuery _                   = False
    inString :: Lexer.Symbol -> Bool
    inString (Lexer.Str    {})           = True
    inString (Lexer.StrEsc {})           = True
    inString (Lexer.Quote _ Lexer.Begin) = True
    inString _                           = False
    findQueryBegin :: [Lexer.Token Lexer.Symbol] -> Int -> Maybe Int
    findQueryBegin []    _ = Nothing
    findQueryBegin (h:t) p = do
        let tokenLength = fromIntegral $ h ^. Lexer.span + h ^. Lexer.offset
        if p > tokenLength
            then (tokenLength +) <$> findQueryBegin t (p - tokenLength)
        else if p <= (fromIntegral $ h ^. Lexer.span)
            then if isQuery (h ^. Lexer.element) then Just 0 else Nothing
        else if inString (h ^. Lexer.element)
            then Nothing
            else Just p
    getInput :: Maybe Int -> Int -> Input
    getInput Nothing    _   = RawInput input'
    getInput (Just beg) caretPos = do
        let (pref', suff') = Text.splitAt caretPos input'
            (pref , q')    = Text.splitAt beg pref'
            (q'', suff)    = Text.breakOn " " suff'
        DividedInput $ Divided pref (q' <> q'') suff

findLambdaArgsAndEndOfLambdaArgs :: Text32 -> [Lexer.Token Lexer.Symbol]
    -> Maybe ([Text], Int)
findLambdaArgsAndEndOfLambdaArgs input' tokens = result where
    result         = findRecursive tokens (0 :: Int) 0 def def
    exprLength   t = fromIntegral $ t ^. Lexer.span
    offsetLength t = fromIntegral $ t ^. Lexer.offset
    getArg   beg t = Vector.slice beg (exprLength t) input'
    tokenLength  t = exprLength t + offsetLength t
    findRecursive []    _                     _      _    res = res
    findRecursive (h:t) openParanthesisNumber endPos args res =
        let findR paranthesisModifier arguments res' = findRecursive
                t
                (paranthesisModifier openParanthesisNumber)
                (endPos + tokenLength h)
                arguments
                res'
            updateResult = Just (convert <$> args, endPos + exprLength h)
            blockStartResult = if openParanthesisNumber == 0
                then updateResult
                else res
            varArgs = getArg endPos h : args
        in case h ^. Lexer.element of
            Lexer.BlockStart             -> findR id   args    blockStartResult
            Lexer.Var        {}          -> findR id   varArgs res
            Lexer.Block      Lexer.Begin -> findR succ args    res
            Lexer.Block      Lexer.End   -> findR pred args    res
            _                            -> findR id   args    res
