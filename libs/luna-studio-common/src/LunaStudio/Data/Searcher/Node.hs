module LunaStudio.Data.Searcher.Node
    ( module LunaStudio.Data.Searcher.Node
    , module X
    ) where

import Searcher.Engine as X hiding (search)

import Prologue hiding (Symbol)

import qualified Data.Char                           as Char
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import qualified Data.Text                           as Text
import qualified Searcher.Engine                     as Searcher
import qualified Searcher.Engine.Data.Symbol         as Symbol
import qualified Searcher.Engine.Data.Symbol.Library as Library

import Control.Lens     (Getter, to)
import Data.Aeson.Types (ToJSON)
import Data.Binary      (Binary)
import Data.List        (find)
import Data.Map         (Map)
import Data.Set         (Set)


type LibraryName = Library.Name

data SymbolPreference = SymbolPreference
    { _symbolName    :: Symbol.Name
    , _symbolLibrary :: LibraryName
    , _symbolKind    :: Symbol.Kind
    , _symbolBonus   :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''SymbolPreference

data TypePreference = TypePreference
    { _localFunctionsWeight         :: Weight
    , _globalFunctionsWeight        :: Weight
    , _specialWeightForClassMethods :: (Set ClassName, Weight)
    , _methodsWeight                :: Weight
    , _constructorsWeight           :: Weight
    } deriving Show

makeLenses ''TypePreference
instance Default TypePreference where def = TypePreference 1 1 (def, 1) 1 1


notConnectedEmptyInputPreference :: [SymbolPreference]
notConnectedEmptyInputPreference =
    [ --SymbolPreference "now" "Std.Time" (Symbol.Method "Time") 100000 
    ]


data ClassHints = ClassHints
    { _constructors :: [(Name, Documentation)]
    , _methods      :: [(Name, Documentation)]
    } deriving (Eq, Generic, Show)

instance Binary ClassHints
instance NFData ClassHints
instance ToJSON ClassHints

data LibraryHints = LibraryHints
    { _functions :: [(Name, Documentation)]
    , _classes   :: Map ClassName ClassHints
    } deriving (Eq, Generic, Show)

makeLenses ''ClassHints
makeLenses ''LibraryHints

instance Binary LibraryHints
instance NFData LibraryHints
instance ToJSON LibraryHints

type LibrariesHintsMap = Map LibraryName LibraryHints

data NodeSearcherData = NodeSearcherData
    { _libraries         :: LibrariesHintsMap
    , _importedLibraries :: Set LibraryName
    } deriving (Eq, Generic, Show)

makeLenses ''NodeSearcherData

instance Binary  NodeSearcherData
instance NFData  NodeSearcherData
instance Default NodeSearcherData where def = NodeSearcherData def def

wildcardName :: Searcher.Name
wildcardName = "_"

wildcardLibrary :: Searcher.Library
wildcardLibrary = Searcher.Library localFunctionsLibraryName True

maxWeight :: Weight
maxWeight = 1000000

maxBonus :: Int
maxBonus = 1000000

wildcardSymbol :: Symbol
wildcardSymbol = Symbol
    wildcardName
    wildcardLibrary
    Symbol.Function
    def
    maxWeight
    maxBonus

wildcardMatch :: Match Symbol
wildcardMatch =
    let matchedChars = [(0, Text.length wildcardName)]
    in Match
        wildcardSymbol
        CaseSensitiveEquality
        maxBonus
        matchedChars

localFunctionsLibraryName :: LibraryName
localFunctionsLibraryName = "Local"

mkLocalFunctionsLibrary :: [Name] -> LibraryHints
mkLocalFunctionsLibrary functionNames = LibraryHints
    ((,def) <$> functionNames)
    def

missingLibraries :: Getter NodeSearcherData (Set LibraryName)
missingLibraries = to missingLibraries' where
    missingLibraries' (NodeSearcherData libs importedLibs) =
        let neededLibraries = Map.keysSet libs
            in Set.filter (`Set.notMember` neededLibraries) importedLibs

isImported :: LibraryName -> NodeSearcherData -> Bool
isImported lib nsData = lib `elem` (nsData ^. importedLibraries)

getLibrary :: LibraryName -> NodeSearcherData -> Library
getLibrary libName nsData = Library libName $ isImported libName nsData



search :: Query -> NodeSearcherData -> Maybe TypePreference -> [Match Symbol]
search q nsData
    = Searcher.search q . nodeSearcherDataToSymbols nsData . fromJust def

notConnectedEmptyInputSearch
    :: Query -> NodeSearcherData -> Maybe TypePreference -> [Match Symbol]
notConnectedEmptyInputSearch q nsData tp = Searcher.search q
    $ toSymbolsWithSymbolsPreference
        nsData
        (fromJust def tp)
        notConnectedEmptyInputPreference


functionsToSymbols :: [(Name, Documentation)] -> Library -> TypePreference
    -> [Symbol]
functionsToSymbols functions' lib typePreference =
    let toSymbol name' doc = Symbol name' lib Symbol.Function doc weight def
        weight = if lib ^. Library.name == localFunctionsLibraryName
            then typePreference  ^. localFunctionsWeight
            else typePreference  ^. globalFunctionsWeight
    in map (uncurry toSymbol) functions'

constructorsToSymbols
    :: [(Name, Documentation)] -> ClassName -> Library -> TypePreference
    -> [Symbol]
constructorsToSymbols constructors' className lib typePreference =
    let weight = typePreference ^. constructorsWeight
        toSymbol name' doc
            = Symbol name' lib (Symbol.Constructor className) doc weight def
    in map (uncurry toSymbol) constructors'

methodsToSymbols
    :: [(Name, Documentation)] -> ClassName -> Library -> TypePreference
    -> [Symbol]
methodsToSymbols methods' className lib typePreference =
    let applySpecialWeight = Set.member className . fst
                $ typePreference ^. specialWeightForClassMethods
        weight = if applySpecialWeight
            then snd $ typePreference ^. specialWeightForClassMethods
            else typePreference ^. methodsWeight
        toSymbol name' doc
            = Symbol name' lib (Symbol.Method className) doc weight def
    in  map (uncurry toSymbol) methods'

classToSymbols
    :: ClassName -> ClassHints -> Library -> TypePreference -> [Symbol]
classToSymbols className hints lib typePreference = 
    let constructors' = constructorsToSymbols
            (hints ^. constructors)
            className
            lib
            typePreference
        methods' = methodsToSymbols
            (hints ^. methods)
            className
            lib
            typePreference
    in constructors' <> methods'


moduleToSymbols :: Library -> LibraryHints -> TypePreference -> [Symbol]
moduleToSymbols lib hints typePreference =
    let functionsSymbols
            = functionsToSymbols (hints ^. functions) lib typePreference
        classesSymbols
            = concat . Map.elems . Map.mapWithKey clToSymbols $ hints ^. classes
        clToSymbols className classHints
            = classToSymbols className classHints lib typePreference
    in functionsSymbols <> classesSymbols

nodeSearcherDataToSymbols :: NodeSearcherData -> TypePreference -> [Symbol]
nodeSearcherDataToSymbols nsData typePreference =
    let toSymbols name' hints
            = moduleToSymbols (getLibrary name' nsData) hints typePreference
    in concat . Map.elems . Map.mapWithKey toSymbols $ nsData ^. libraries

matchesSymbolPreference :: Symbol -> SymbolPreference -> Bool
matchesSymbolPreference symbol sp =
    let nameMatches = sp ^. symbolName  == symbol ^. Symbol.name
        libMatches
            = sp ^. symbolLibrary == symbol ^. Symbol.library . Library.name
        kindMatches = sp ^. symbolKind  == symbol ^. Symbol.kind
    in nameMatches && libMatches && kindMatches

toSymbolsWithSymbolsPreference
    :: NodeSearcherData -> TypePreference -> [SymbolPreference] -> [Symbol]
toSymbolsWithSymbolsPreference nsData typePreference symbolPreferences =
    let symbols = nodeSearcherDataToSymbols nsData typePreference
        getAdjustedBonus s = view symbolBonus
            <$> (find (matchesSymbolPreference s) symbolPreferences)
        adjust s = maybe
            s
            (\b -> s & Symbol.initialBonus .~ b)
            $ getAdjustedBonus s
    in map adjust symbols


getWeights :: Bool -> Bool -> Maybe ClassName  -> Query -> TypePreference
getWeights isFirstSearch searchForMethodsOnly mayClassName q = do
    let expectingConstructor = not (Text.null q) && Char.isUpper (Text.head q)
        constructorWeight ifExpecting ifNotExpecting
            = if expectingConstructor then ifExpecting else ifNotExpecting
    if searchForMethodsOnly
        then TypePreference 0 0 (def, def) 1 0
    else if not isFirstSearch
        then TypePreference 0.7 0.5 (def, def) 0.3 $ constructorWeight 0.6 0.1
    else case mayClassName of
        Just cn -> TypePreference 0.2 0.3 (Set.singleton cn, 0.7) 0.5 0.1
        _       -> TypePreference 0.5 0.7 (def, def) 0.3 $ constructorWeight 0.9 0.2

