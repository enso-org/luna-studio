module LunaStudio.Data.NodeSearcher
    ( module LunaStudio.Data.NodeSearcher
    , RawEntry (..)
    , Match (..)
    , EntryType (..)
    , ClassName
    , ImportName
    , ImportInfo(..)
    , Range
    , Query
    , Score
    , name
    , doc
    , entryType
    , weight
    , score
    , match
    , exactMatch
    , className
    , importName
    , imported
    , importInfo
    ) where

import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           FuzzyText        (ClassName, EntryType (..), ImportInfo (ImportInfo), ImportName, Match (..), Query, Range, RawEntry (..),
                                   Score, className, doc, entryType, exactMatch, fuzzySearch, importInfo, importName, imported, match, name,
                                   score, weight)
import           Prologue         hiding (Item)

type Name = Text
type Doc  = Text

data ClassHints = ClassHints
    { _constructors :: [(Name, Doc)]
    , _methods      :: [(Name, Doc)]
    } deriving (Eq, Generic, Show)

data ModuleHints = ModuleHints
    { _functions    :: [(Name, Doc)]
    , _classes      :: Map Text ClassHints
    } deriving (Eq, Generic, Show)

makeLenses ''ClassHints
makeLenses ''ModuleHints

instance Binary ModuleHints
instance NFData ModuleHints
instance ToJSON ModuleHints
instance Binary ClassHints
instance NFData ClassHints
instance ToJSON ClassHints

type ImportsHints = Map ImportName ModuleHints

data NodeSearcherData = NodeSearcherData
    { _imports        :: Map ImportName ModuleHints
    , _currentImports :: Set ImportName
    } deriving (Eq, Generic, Show)

makeLenses ''NodeSearcherData

instance Binary  NodeSearcherData
instance NFData  NodeSearcherData
instance Default NodeSearcherData where def = NodeSearcherData def def

missingImports :: Getter NodeSearcherData (Set ImportName)
missingImports = to missingImports' where
    missingImports' (NodeSearcherData imps currentImps) =
        let neededImports = Map.keysSet imps
            in Set.filter (`Set.notMember` neededImports) currentImps


data TypePreferation = TypePreferation
    { _localFunctionsWeight         :: Double
    , _globalFunctionsWeight        :: Double
    , _specialWeightForClassMethods :: (Set ClassName, Double)
    , _methodsWeight                :: Double
    , _constructorsWeight           :: Double
    } deriving Show

makeLenses ''TypePreferation
instance Default TypePreferation where def = TypePreferation 1 1 (def, 1) 1 1

searchCommands :: Query -> [Text] -> [Match]
searchCommands q = fuzzySearch q . fmap (\c -> RawEntry c def Command 1 def)

search :: Query -> NodeSearcherData -> Maybe TypePreferation -> [Match]
search q nsData tp = fuzzySearch q $ toEntries nsData (fromJust def tp)


toEntries :: NodeSearcherData -> TypePreferation -> [RawEntry]
toEntries (NodeSearcherData ih currImps) tPref
    = concat . Map.elems $ Map.mapWithKey moduleHintsToEntries ih where
        moduleHintsToEntries :: ImportName -> ModuleHints -> [RawEntry]
        moduleHintsToEntries impName mh
            =  classesToEntries impName (mh ^. classes)
            <> functionsToEntries impName (mh ^. functions)
        classesToEntries :: ImportName -> Map Text ClassHints -> [RawEntry]
        classesToEntries impName
            = concat . Map.elems . Map.mapWithKey (classToEntries impName)
        classToEntries :: ImportName -> ClassName -> ClassHints -> [RawEntry]
        classToEntries impName cName c
            =  methodsToEntries impName cName (c ^. methods)
            <> constructorsToEntries impName cName (c ^. constructors)
        methodsToEntries :: ImportName -> ClassName -> [(Name, Doc)]
            -> [RawEntry]
        methodsToEntries impName cName
            = fmap (uncurry $ methodToEntry impName cName)
        methodToEntry :: ImportName -> ClassName -> Name -> Doc -> RawEntry
        methodToEntry impName cName m d
            = let et = Method cName in
                RawEntry
                    m
                    d
                    et
                    (getWeight impName cName et)
                    $ Just $ ImportInfo impName $ isImported impName
        constructorsToEntries :: ImportName -> ClassName -> [(Name, Doc)]
            -> [RawEntry]
        constructorsToEntries impName cName
            = fmap (uncurry $ constructorToEntry impName cName)
        constructorToEntry :: ImportName -> ClassName -> Name -> Doc -> RawEntry
        constructorToEntry impName cName c d
            = let et = Constructor cName in
                RawEntry
                    c
                    d
                    et
                    (getWeight impName cName et)
                    $ Just $ ImportInfo impName $ isImported impName
        functionsToEntries :: ImportName -> [(Name, Doc)] -> [RawEntry]
        functionsToEntries impName = fmap (uncurry $ functionToEntry impName)
        functionToEntry :: ImportName -> Name -> Doc -> RawEntry
        functionToEntry impName f d
            = RawEntry
                f
                d
                Function
                (getWeight impName def Function)
                $ Just $ ImportInfo impName $ isImported impName
        isImported :: ImportName -> Bool
        isImported impName = impName `elem` currImps
        getWeight :: ImportName -> ClassName -> EntryType -> Double
        getWeight moduleName cName et = case et of
            Function -> if moduleName == "Local"
                then tPref ^. localFunctionsWeight
                else tPref ^. globalFunctionsWeight
            Method _
                -> let applySpecialWeight = Set.member cName . fst
                        $ tPref ^. specialWeightForClassMethods
                in if applySpecialWeight
                    then snd $ tPref ^. specialWeightForClassMethods
                    else tPref ^. methodsWeight
            Constructor _ -> tPref ^. constructorsWeight
            _             -> def
