{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Node where


import Common.Prelude

import qualified Data.Map.Strict                       as Map
import qualified Data.Set                              as Set
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as Class
import qualified LunaStudio.Data.Searcher.Hint.Library as Library
import qualified Searcher.Engine.Data.Database         as Database

import Data.Map.Strict                       (Map)
import Data.Set                              (Set)
import LunaStudio.Data.Searcher.Hint         (SearcherHint)
import LunaStudio.Data.Searcher.Hint.Class   (Class)
import LunaStudio.Data.Searcher.Hint.Library (Library (Library),
                                              SearcherLibraries)
import Searcher.Engine.Data.Database         (SearcherData (fixedScore, text))
import Searcher.Engine.Data.Score            (Score)



------------------
-- === Kind === --
------------------


-- === Definition === --

data Kind
    = Function
    | Constructor Class.Name
    | Method      Class.Name
    deriving (Eq, Generic, Show)

makePrisms ''Kind

instance NFData Kind

className :: Getter Kind (Maybe Class.Name)
className = to $! \case
    Function       -> Nothing
    Constructor cn -> Just cn
    Method      cn -> Just cn
{-# INLINE className #-}

------------------
-- === Node === --
------------------


-- === Definition === --

data Node = Node
    { _expression        :: Text
    , _library           :: Library.Info
    , _kind              :: Kind
    , _documentationText :: Text
    , _fixedBonus        :: Score
    } deriving (Eq, Generic, Show)

makeLenses ''Node

instance NFData Node
instance SearcherData Node where
    text       = expression
    fixedScore = to $ \n -> let
        fixed       = n ^. fixedBonus
        libImported = n ^. library . Library.imported
        importBonus = if libImported then libraryImportedBonus else 0
        in fixed + importBonus
instance SearcherHint Node where
    prefix        = kind . className . to (fromJust mempty)
    documentation = documentationText


-- === API === --

libraryImportedBonus :: Score
libraryImportedBonus = 100
{-# INLINE libraryImportedBonus #-}

fromRawHint :: Hint.Raw -> Library.Info -> Kind -> Node
fromRawHint raw libInfo kind' = let
    expr = raw ^. Database.text
    doc  = raw ^. Hint.documentation
    in Node expr libInfo kind' doc def
{-# INLINE fromRawHint #-}

fromFunction :: Hint.Raw -> Library.Info -> Node
fromFunction raw libInfo = fromRawHint raw libInfo Function
{-# INLINE fromFunction #-}

fromMethod :: Hint.Raw -> Class.Name -> Library.Info -> Node
fromMethod raw className libInfo = fromRawHint raw libInfo $! Method className
{-# INLINE fromMethod #-}

fromConstructor :: Hint.Raw -> Class.Name -> Library.Info -> Node
fromConstructor raw className libInfo
    = fromRawHint raw libInfo $! Constructor className
{-# INLINE fromConstructor #-}


fromClass :: Class.Name -> Class -> Library.Info -> [Node]
fromClass className klass libInfo = constructorsHints <> methodsHints where
    constructors = klass ^. Class.constructors
    methods      = klass ^. Class.methods
    fromConstructor' h = fromConstructor h className libInfo
    fromMethod'      h = fromMethod      h className libInfo
    constructorsHints  = fromConstructor' <$> constructors
    methodsHints       = fromMethod'      <$> methods
{-# INLINE fromClass #-}


fromLibrary :: Library -> Library.Info -> [Node]
fromLibrary lib libInfo = functionsHints <> classesHints where
    functionsHints = flip fromFunction libInfo <$> lib ^. Library.functions
    appendClass acc className klass = acc <> fromClass className klass libInfo
    classesHints = Map.foldlWithKey appendClass mempty $! lib ^. Library.classes
{-# INLINE fromLibrary #-}

fromSearcherLibraries :: SearcherLibraries -> Set Library.Name -> [Node]
fromSearcherLibraries libs importedLibs = let
    toLibInfo libName = Library.Info libName $! Set.member libName importedLibs
    appendLib acc libName lib = acc <> fromLibrary lib (toLibInfo libName)
    in Map.foldlWithKey appendLib mempty libs
{-# INLINE fromSearcherLibraries #-}



----------------------
-- === Database === --
----------------------


-- === Definition === --

data Database = Database
    { _database :: Database.Database Node
    , _imported :: Set Library.Name
    } deriving (Eq, Generic, Show)

makeLenses ''Database

instance NFData  Database
instance Default Database where def = Database def def


-- === API === --

missingLibraries :: Getter Database (Set Library.Name)
missingLibraries = to $ \d -> let
    allHints         = d ^. database . Database.hints . to Map.elems . to concat
    addLibName acc h = Set.insert (h ^. library . Library.name) acc
    presentLibs      = foldl addLibName mempty allHints
    importedLibs     = d ^. imported
    in Set.difference importedLibs presentLibs
{-# INLINE missingLibraries #-}

localFunctionsLibraryName :: Text
localFunctionsLibraryName = "Local"
{-# INLINE localFunctionsLibraryName #-}

insertHints :: [Node] -> Database -> Database
insertHints v d = d & database %~ Database.insertMultiple v
{-# INLINE insertHints #-}

insertSearcherLibraries :: SearcherLibraries -> Database -> Database
insertSearcherLibraries libs d = let
    importedLibs = d ^. imported
    nodeHints    = fromSearcherLibraries libs importedLibs
    in d & database %~ Database.insertMultiple nodeHints
{-# INLINE insertSearcherLibraries #-}

