{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Node where


import Common.Prelude

import qualified Data.Map.Strict                       as Map
import qualified Data.Set                              as Set
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as Class
import qualified LunaStudio.Data.Searcher.Hint.Library as Library
import qualified Searcher.Data.Class                   as SearcherData
import qualified Searcher.Data.Database                as Searcher

import Data.Set                              (Set)
import LunaStudio.Data.Searcher.Hint.Class   (Class)
import LunaStudio.Data.Searcher.Hint.Library (Library)
import Searcher.Data.Class                   (SearcherData, SearcherHint)



------------------
-- === Kind === --
------------------

-- === Definition === --

data Kind
    = Function
    | Constructor Class.Name
    | Method      Class.Name
    | Snippet     (Maybe Class.Name)
    deriving (Eq, Generic, Show)

makePrisms ''Kind

instance NFData Kind

className :: Getter Kind (Maybe Class.Name)
className = to $ \case
    Function       -> Nothing
    Constructor cn -> Just cn
    Method      cn -> Just cn
    Snippet     cn -> cn
{-# INLINE className #-}

------------------
-- === Node === --
------------------

-- === Definition === --

data Node = Node
    { _expression    :: Text
    , _library       :: Library.Info
    , _kind          :: Kind
    , _documentation :: Text
    , _tags          :: [Text]
    , _priority      :: Maybe Int
    } deriving (Eq, Generic, Show)

makeLenses ''Node

instance NFData Node
instance SearcherData Node where
    text     = expression
    tags     = tags
    priority = priority
instance SearcherHint Node where
    prefix        = kind . className . to (fromMaybe mempty)
    documentation = documentation

-- === API === --

fromRawHint :: Hint.Raw -> Library.Info -> Kind -> Node
fromRawHint raw libInfo kind = let
    expr     = raw ^. Hint.name
    doc      = raw ^. Hint.documentation
    tags     = raw ^. Hint.tags
    priority = raw ^. Hint.priority
    in Node expr libInfo kind doc tags priority
{-# INLINE fromRawHint #-}

fromFunction :: Library.Info -> Hint.Raw -> Node
fromFunction libInfo raw = fromRawHint raw libInfo Function
{-# INLINE fromFunction #-}

fromMethod :: Class.Name -> Library.Info -> Hint.Raw -> Node
fromMethod className libInfo raw = fromRawHint raw libInfo $ Method className
{-# INLINE fromMethod #-}

fromConstructor :: Class.Name -> Library.Info -> Hint.Raw -> Node
fromConstructor className libInfo raw
    = fromRawHint raw libInfo $ Constructor className
{-# INLINE fromConstructor #-}

fromSnippet :: Maybe Class.Name -> Library.Info -> Hint.Raw -> Node
fromSnippet className libInfo raw = fromRawHint raw libInfo $ Snippet className
{-# INLINE fromSnippet #-}

fromClass :: Class.Name -> Class -> Library.Info -> [Node]
fromClass className klass libInfo = let
    constructors = klass ^. Class.constructors
    methods      = klass ^. Class.methods
    snippets     = klass ^. Class.snippets
    constructorsHints  = fromConstructor className libInfo <$> constructors
    methodsHints       = fromMethod      className libInfo <$> methods
    snippetHints       = fromSnippet (Just className) libInfo <$> snippets
    in concat [constructorsHints, methodsHints, snippetHints]
{-# INLINE fromClass #-}

fromLibrary :: Library -> Library.Info -> [Node]
fromLibrary lib libInfo = let
    functionsHints = fromFunction libInfo <$> lib ^. Library.functions
    processClass className klass = fromClass className klass libInfo
    globalSnippets = lib ^. Library.globalSnippets
    globalSnippetHints = fromSnippet Nothing libInfo <$> globalSnippets
    importedSnippetsHints = if libInfo ^. Library.imported
        then fromSnippet Nothing libInfo <$> lib ^. Library.importedSnippets
        else mempty
    classes = lib ^. Library.classes
    classesHints = concatMap (uncurry processClass) . Map.toList $ classes
    in concat [functionsHints,
               classesHints,
               globalSnippetHints,
               importedSnippetsHints]
{-# INLINE fromLibrary #-}

fromSearcherLibraries :: Library.Set -> Set Library.Name -> [Node]
fromSearcherLibraries libs importedLibs = let
    toLibInfo libName = Library.Info libName $ Set.member libName importedLibs
    processLib libName lib = fromLibrary lib (toLibInfo libName)
    in concatMap (uncurry processLib) . Map.toList $ libs
{-# INLINE fromSearcherLibraries #-}


----------------------
-- === Database === --
----------------------

-- === Definition === --

data Database = Database
    { _database :: Searcher.Database Node
    , _imported :: Set Library.Name
    , _bareLibs :: Library.Set
    } deriving (Generic)

makeLenses ''Database

instance Default Database where
    def = Database def def def

-- === API === --

missingLibraries :: Getter Database (Set Library.Name)
missingLibraries = to $ \db -> let
    presentLibs  = Set.fromList . Map.keys $ db ^. bareLibs
    importedLibs = db ^. imported
    in Set.difference importedLibs presentLibs
{-# INLINE missingLibraries #-}

localFunctionsLibraryName :: Text
localFunctionsLibraryName = "#local"
{-# INLINE localFunctionsLibraryName #-}

mkLocalFunctionsDb :: [Text] -> Database
mkLocalFunctionsDb syms = insertSearcherLibraries libs def where
    libs    = Map.singleton localFunctionsLibraryName library
    library = Library.Library hints def def def
    hints   = Hint.mk <$> syms

insertSearcherLibraries :: Library.Set -> Database -> Database
insertSearcherLibraries libs db = let
    oldLibraries = db ^. bareLibs
    importedLibs = db ^. imported
    libs'        = Map.union libs oldLibraries
    nodeHints    = fromSearcherLibraries libs' importedLibs
    searcherDb   = Searcher.create nodeHints
    in Database searcherDb importedLibs libs'
{-# INLINE insertSearcherLibraries #-}

importLibrary :: Library.Name -> Database -> Database
importLibrary lib db = let
    importedSoFar = db ^. imported
    libsHints = db ^. bareLibs
    newDb = Database def (Set.insert lib importedSoFar) def
    in insertSearcherLibraries libsHints newDb

setImportedLibraries :: Set Library.Name -> Database -> Database
setImportedLibraries libs db = let
    libsHints = db ^. bareLibs
    newDb = Database def libs def
    in insertSearcherLibraries libsHints newDb


