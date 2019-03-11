{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Node where


import Common.Prelude

import qualified Data.Map.Strict                       as Map
import qualified Data.Set                              as Set
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as Class
import qualified LunaStudio.Data.Searcher.Hint.Library as Library
import qualified Searcher.Data.Database                as Searcher

import Data.Set                              (Set)
import LunaStudio.Data.Searcher.Hint.Class   (Class)
import LunaStudio.Data.Searcher.Hint.Library (Library)
import Searcher.Data.Class                   (SearcherData (text),
                                              SearcherHint (prefix,
                                                            documentation))



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
    } deriving (Eq, Generic, Show)

makeLenses ''Node

instance NFData Node
instance SearcherData Node where
    text       = expression
instance SearcherHint Node where
    prefix        = kind . className . to (fromMaybe mempty)
    documentation = documentationText


-- === API === --

fromRawHint :: Hint.Raw -> Library.Info -> Kind -> Node
fromRawHint raw libInfo kind' = let
    expr = raw ^. Hint.name
    doc  = raw ^. Hint.documentationText
    in Node expr libInfo kind' doc
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
    processClass className klass = fromClass className klass libInfo
    classes = lib ^. Library.classes
    classesHints = concat $ fmap (uncurry processClass) $ Map.toList $ classes
{-# INLINE fromLibrary #-}

fromSearcherLibraries :: Library.Set -> Set Library.Name -> [Node]
fromSearcherLibraries libs importedLibs = let
    toLibInfo libName = Library.Info libName $! Set.member libName importedLibs
    processLib libName lib = fromLibrary lib (toLibInfo libName)
    in concat $ fmap (uncurry processLib) $ Map.toList libs
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
missingLibraries = to $ \d -> let
    allHints         = Searcher.elems $ d ^. database
    addLibName acc h = Set.insert (h ^. library . Library.name) acc
    presentLibs      = foldl addLibName mempty allHints
    importedLibs     = d ^. imported
    in Set.difference importedLibs presentLibs
{-# INLINE missingLibraries #-}

localFunctionsLibraryName :: Text
localFunctionsLibraryName = "#local"
{-# INLINE localFunctionsLibraryName #-}

mkLocalFunctionsDb :: [Text] -> Database
mkLocalFunctionsDb syms = insertSearcherLibraries libs def where
    libs    = Map.singleton localFunctionsLibraryName library
    library = Library.Library hints def
    hints   = flip Hint.Raw mempty <$> syms

insertSearcherLibraries :: Library.Set -> Database -> Database
insertSearcherLibraries libs d = let
    oldLibraries = d ^. bareLibs
    importedLibs = d ^. imported
    libs'        = Map.union libs oldLibraries
    nodeHints    = fromSearcherLibraries libs' importedLibs
    db           = Searcher.create nodeHints
    in Database db importedLibs libs'
{-# INLINE insertSearcherLibraries #-}
