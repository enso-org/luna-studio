module Empire.Commands.Graph.SearcherHints where

import Prologue

import qualified Control.Monad.Exception.IO            as Exception
import qualified Data.Bimap                            as Bimap
import qualified Data.Map                              as Map
import qualified Data.Yaml                             as Yaml
import qualified Luna.Datafile.Stdlib                  as StdLocator
import qualified Luna.IR                               as IR
import qualified Luna.Package                          as Package
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.UnitLoader         as UnitLoader
import qualified Luna.Pass.Sourcing.UnitMapper         as UnitMapper
import qualified Luna.Std                              as Std
import qualified LunaStudio.Data.GraphLocation         as GraphLocation
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as SearcherClass
import qualified LunaStudio.Data.Searcher.Hint.Library as SearcherLibrary
import qualified Path

import Control.Arrow                 ((***))
import Control.Monad.Catch           (try)
import Control.Monad.Exception       (MonadException)
import Data.Map                      (Map)
import Empire.ASTOp                  (liftScheduler)
import Empire.Data.AST               (astExceptionFromException,
                                      astExceptionToException)
import Empire.Empire                 (Empire)
import Luna.Datafile                 (DatafileException)
import Luna.Package                  (PackageNotFoundException)
import Luna.Pass.Data.Stage          (Stage)
import LunaStudio.Data.GraphLocation (GraphLocation)

data ModuleCompilationException
    = ModuleCompilationException UnitLoader.UnitLoadingError
    deriving (Show)

instance Exception ModuleCompilationException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

snippetsFileName :: FilePath
snippetsFileName = "snippets.yaml"

globalSnippetsFieldName :: Text
globalSnippetsFieldName = "$global"

importedSnippetsFieldName :: Text
importedSnippetsFieldName = "$imported"

getImportPaths ::
    (MonadIO m, MonadException PackageNotFoundException m,
     MonadException DatafileException m,
     MonadException Path.PathException m
     ) => GraphLocation -> m [FilePath]
getImportPaths location = do
    let file = location ^. GraphLocation.filePath
    stdPath         <- StdLocator.findPath
    filePath        <- Exception.rethrowFromIO @Path.PathException
                                               (Path.parseAbsFile file)
    currentProjPath <- Package.packageRootForFile filePath
    importPaths     <- Package.packageImportPaths currentProjPath stdPath
    pure $ snd <$> importPaths

getSnippetsFile :: MonadIO m => FilePath -> m (Map Text (Map Text [Hint.Raw]))
getSnippetsFile projectRoot = do
    contents <- liftIO $ Yaml.decodeFileEither (projectRoot <> snippetsFileName)
    case contents of
        Left err -> pure def
        Right f  -> pure f

isPublicMethod :: IR.Name -> Bool
isPublicMethod (convert -> n) = head n /= Just '_'

addSnippetsToLibrary :: Map Text [Hint.Raw] -> SearcherLibrary.Library
                     -> SearcherLibrary.Library
addSnippetsToLibrary snippets lib = let
    globalSnippets =
        Map.findWithDefault mempty globalSnippetsFieldName snippets
    importedSnippets =
        Map.findWithDefault mempty importedSnippetsFieldName snippets
    processClass clsName =
        SearcherClass.snippets .~ Map.findWithDefault mempty clsName snippets
    classes = lib ^. SearcherLibrary.classes
    classesWithSnippets = Map.mapWithKey processClass classes
    in lib & SearcherLibrary.globalSnippets   .~ globalSnippets
           & SearcherLibrary.importedSnippets .~ importedSnippets
           & SearcherLibrary.classes          .~ classesWithSnippets

addSnippets :: Map Text (Map Text [Hint.Raw]) -> SearcherLibrary.Set
            -> SearcherLibrary.Set
addSnippets snippets libraries = let
    processLib libName =
        addSnippetsToLibrary (Map.findWithDefault def libName snippets)
    libsWithSnippets = Map.mapWithKey processLib libraries
    in libsWithSnippets

importsToHints :: Unit.Unit -> SearcherLibrary.Library
importsToHints (Unit.Unit definitions classes) = let
    funToHint (name, def) = Hint.Raw (convert name) $ fromJust mempty
                                                    $ def ^. Def.documentation
    funHints   = funToHint <$> Map.toList (unwrap definitions)
    classHints = classToHints . view Def.documented <$> classes
    in SearcherLibrary.Library funHints
                               (Map.mapKeys convert classHints)
                               mempty
                               mempty

classToHints :: Class.Class -> SearcherClass.Class
classToHints (Class.Class constructors methods _) = let
    getDocumentation    = fromJust mempty . view Def.documentation
    constructorsNames   = Map.keys constructors
    constructorToHint   = flip Hint.Raw mempty . convert
    constructorsHints   = constructorToHint <$> constructorsNames
    methods'            = filter (isPublicMethod . fst)
                        . Map.toList $ unwrap methods
    methodToHint (n, d) = Hint.Raw (convert n) $ getDocumentation d
    methodsHints        = methodToHint <$> methods'
    in SearcherClass.Class constructorsHints methodsHints mempty

readUnits :: Map IR.Qualified FilePath -> Empire (Map IR.Qualified Unit.Unit)
readUnits sourceFiles = do
    result <- try $ liftScheduler $ do
        UnitLoader.init
        unitRefs <- Map.traverseWithKey (flip UnitLoader.readUnit) sourceFiles
        units    <- flip Map.traverseWithKey unitRefs $ \name unitRef ->
            case unitRef ^. Unit.root of
                Unit.Graph termUnit   -> UnitMapper.mapUnit name termUnit
                Unit.Precompiled unit -> pure unit
        pure units
    case result of
        Left exc    -> throwM $ ModuleCompilationException exc
        Right units -> pure units

getSearcherHints :: GraphLocation -> Empire SearcherLibrary.Set
getSearcherHints loc = do
    importPaths     <- getImportPaths loc
    availableSource <- for importPaths $ \path -> do
        sources <- Package.findPackageSources =<< Path.parseAbsDir path
        pure $ Bimap.toMapR sources
    let allSources = Path.toFilePath <$> Map.unions availableSource
    units        <- readUnits allSources
    snippetFiles <- Map.unions <$> traverse getSnippetsFile importPaths
    let sourceHints = Map.fromList
                    $ fmap (convert *** importsToHints)
                    $ Map.toList units
        hintsWithSnippets = addSnippets snippetFiles sourceHints
    pure hintsWithSnippets

