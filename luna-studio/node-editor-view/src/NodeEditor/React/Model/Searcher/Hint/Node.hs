{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher.Hint.Node where


import Common.Prelude

import qualified Data.Array                            as Array
import qualified Data.Map.Strict                       as Map
import qualified Data.Set                              as Set
import qualified JS.SearcherEngine                     as Searcher
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as Class
import qualified LunaStudio.Data.Searcher.Hint.Library as Library
import qualified Searcher.Engine.Data.Database         as Database

import Data.Array                            (Array)
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
    prefix        = kind . className . to (fromMaybe mempty)
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
    {-appendClass acc className klass = acc <> fromClass className klass libInfo-}
    processClass className klass = fromClass className klass libInfo
    {-classesHints = Map.foldlWithKey appendClass mempty $! lib ^. Library.classes-}
    classesHints = concat $ fmap (uncurry processClass) $ Map.toList $ lib ^. Library.classes
{-# INLINE fromLibrary #-}

fromSearcherLibraries :: SearcherLibraries -> Set Library.Name -> [Node]
fromSearcherLibraries libs importedLibs = let
    toLibInfo libName = Library.Info libName $! Set.member libName importedLibs
    {-appendLib acc libName lib = acc <> fromLibrary lib (toLibInfo libName)-}
    processLib libName lib = fromLibrary lib (toLibInfo libName)
    {-in Map.foldlWithKey appendLib mempty libs-}
    in concat $ fmap (uncurry processLib) $ Map.toList libs
{-# INLINE fromSearcherLibraries #-}



----------------------
-- === Database === --
----------------------


-- === Definition === --

data Database = Database
    { _database :: Searcher.Database
    , _imported :: Set Library.Name
    , _bareLibs :: SearcherLibraries
    , _nodes    :: Array Int Node
    } deriving (Generic)

makeLenses ''Database

instance Default Database where
    def = Database def def def (Array.listArray (0, -1) [])

-- === API === --

missingLibraries :: Getter Database (Set Library.Name)
missingLibraries = to $ \d -> let
    allHints         = Array.elems $ d ^. nodes
    addLibName acc h = Set.insert (h ^. library . Library.name) acc
    presentLibs      = foldl addLibName mempty allHints
    importedLibs     = d ^. imported
    in Set.difference importedLibs presentLibs
{-# INLINE missingLibraries #-}

localFunctionsLibraryName :: Text
localFunctionsLibraryName = "Local"
{-# INLINE localFunctionsLibraryName #-}

insertSearcherLibraries :: MonadIO m => SearcherLibraries -> Database -> m Database
insertSearcherLibraries libs d = do
    let oldLibraries = d ^. bareLibs
        importedLibs = d ^. imported
        libs'        = Map.union libs oldLibraries
        nodeHints    = fromSearcherLibraries libs' importedLibs
        nodeHintsLen = length nodeHints
        nodesArr     = Array.listArray (0, nodeHintsLen - 1) nodeHints
        nodesAssocs  = (_2 %~ view expression) <$> Array.assocs nodesArr
    db <- liftIO $ Searcher.createDatabase nodesAssocs
    pure $ Database db importedLibs libs' nodesArr
{-# INLINE insertSearcherLibraries #-}

