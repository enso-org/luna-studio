module NodeEditor.React.Model.Searcher.Hint
    ( module NodeEditor.React.Model.Searcher.Hint
    , module X
    ) where

import Searcher.Engine            as X (Command, Match, MatchKind (AllCharsMatched, CaseInsensitiveEquality, CaseSensitiveEquality, NotFullyMatched),
                                        Name, Prefix, Range, Symbol)
import Searcher.Engine.Data.Match as X (IsMatch (matchKind, matchedCharacters), SearcherData (documentation, hintTextSeparator, name, prefix, rawDocumentation, score))

import Common.Prelude


data Hint
    = CommandHint (Match Command)
    | NodeHint    (Match Symbol)
    deriving (Eq, Generic, Show)

makePrisms ''Hint


instance SearcherData Hint where
    name =
        let getter (CommandHint h) = h ^. name
            getter (NodeHint    h) = h ^. name
            setter (CommandHint h) n = CommandHint $ h & name .~ n
            setter (NodeHint    h) n = NodeHint    $ h & name .~ n
        in lens getter setter
    rawDocumentation =
        let getter (CommandHint h) = h ^. rawDocumentation
            getter (NodeHint    h) = h ^. rawDocumentation
            setter (CommandHint h) d
                = CommandHint $ h & rawDocumentation .~ d
            setter (NodeHint   h) d
                = NodeHint   $ h & rawDocumentation .~ d
        in lens getter setter
    prefix =
        let getter (CommandHint h) = h ^. prefix
            getter (NodeHint    h) = h ^. prefix
        in to getter
    score =
        let getter (CommandHint h) = h ^. score
            getter (NodeHint    h) = h ^. score
        in to getter
    hintTextSeparator =
        let getter (CommandHint h) = h ^. hintTextSeparator
            getter (NodeHint    h) = h ^. hintTextSeparator
        in to getter

instance IsMatch Hint where
    matchKind =
        let getter (CommandHint h) = h ^. matchKind
            getter (NodeHint    h) = h ^. matchKind
        in to getter
    matchedCharacters =
        let getter (CommandHint h) = h ^. matchedCharacters
            getter (NodeHint    h) = h ^. matchedCharacters
        in to getter
