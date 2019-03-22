module Empire.Commands.Graph.SearcherHints where

import Prologue

import qualified Control.Monad.Exception.IO as Exception
import qualified Data.Bimap                            as Bimap
import qualified Data.Map                              as Map
import qualified Luna.Datafile.Stdlib               as StdLocator
import qualified Luna.IR                               as IR
import qualified Luna.Package                          as Package
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.UnitLoader         as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper         as UnitMapper
import qualified Luna.Std                              as Std
import qualified LunaStudio.Data.GraphLocation         as GraphLocation
import qualified LunaStudio.Data.Searcher.Hint         as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class   as SearcherClass
import qualified LunaStudio.Data.Searcher.Hint.Library as SearcherLibrary
import qualified Path
import qualified Data.Yaml as Yaml

import Control.Monad.Catch           (try)
import Data.Map (Map)
import Empire.ASTOp                  (liftScheduler)
import Empire.Data.AST               (astExceptionFromException,
                                      astExceptionToException)
import Empire.Empire                 (Empire)
import Luna.Pass.Data.Stage          (Stage)
import LunaStudio.Data.GraphLocation (GraphLocation)
import Luna.Package        (PackageNotFoundException)
import Luna.Datafile (DatafileException)
import Control.Monad.Exception (MonadException)

data ModuleCompilationException
    = ModuleCompilationException ModLoader.UnitLoadingError
    deriving (Show)

instance Exception ModuleCompilationException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

snippetsFileName :: FilePath
snippetsFileName = "snippets.yaml"

globalSnippetsFieldName :: Text
globalSnippetsFieldName = "$global"

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

addSnippetsToLibrary :: SearcherLibrary.Library -> Map Text [Hint.Raw] -> SearcherLibrary.Library
addSnippetsToLibrary lib snippets = let
    globalSnippets = Map.findWithDefault mempty globalSnippetsFieldName snippets
    processClass clsName cls = cls & SearcherClass.snippets .~ Map.findWithDefault mempty clsName snippets
    classes = lib ^. SearcherLibrary.classes
    classesWithSnippets = Map.mapWithKey processClass classes
    in lib & SearcherLibrary.snippets .~ globalSnippets
           & SearcherLibrary.classes  .~ classesWithSnippets



addSnippets :: SearcherLibrary.Set -> Map Text (Map Text [Hint.Raw]) -> SearcherLibrary.Set
addSnippets libraries snippets = let
    processLib libName lib = addSnippetsToLibrary lib (Map.findWithDefault def libName snippets)
    libsWithSnippets = Map.mapWithKey processLib libraries
    in libsWithSnippets

importsToHints :: Unit.Unit -> SearcherLibrary.Library
importsToHints (Unit.Unit definitions classes) = let
    funToHint (n,d) = Hint.Raw
        (convert n)
        $ fromMaybe mempty $ d ^. Def.documentation
    funHints   = funToHint <$> Map.toList (unwrap definitions)
    classHints = (classToHints . view Def.documented) <$> classes
    in SearcherLibrary.Library funHints (Map.mapKeys convert classHints) []

classToHints :: Class.Class -> SearcherClass.Class
classToHints (Class.Class constructors methods _) = let
    getDocumentation    = fromMaybe mempty . view Def.documentation
    constructorsNames   = Map.keys constructors
    constructorToHint   = flip Hint.Raw mempty . convert
    constructorsHints   = constructorToHint <$> constructorsNames
    methods'            = filter (isPublicMethod . fst)
                        . Map.toList $ unwrap methods
    methodToHint (n, d) = Hint.Raw (convert n) $ getDocumentation d
    methodsHints        = methodToHint <$> methods'
    in SearcherClass.Class constructorsHints methodsHints []

getSearcherHints :: GraphLocation -> Empire SearcherLibrary.Set
getSearcherHints loc = do
    importPaths     <- liftIO $ getImportPaths loc
    availableSource <- liftIO $ for importPaths $ \path -> do
        sources <- Package.findPackageSources =<< Path.parseAbsDir path
        return $ Bimap.toMapR sources
    let allSourceFiles = Map.map (Path.toFilePath) $ Map.unions availableSource
    res <- try $ liftScheduler $ do
        ModLoader.init
        stdUnitRef <- snd <$> Std.stdlib @Stage
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr @Unit.UnitRefsMap $ wrap $ Map.singleton "Std.Primitive" stdUnitRef
        for (Map.keys allSourceFiles) $ ModLoader.loadUnit def allSourceFiles []
        refsMap <- Scheduler.getAttr @Unit.UnitRefsMap
        units <- flip Map.traverseWithKey (unwrap refsMap) $ \name unitRef -> case unitRef ^. Unit.root of
            Unit.Graph termUnit   -> UnitMapper.mapUnit name termUnit
            Unit.Precompiled unit -> return unit
        return units
    snippetFiles <- fmap Map.unions $ traverse getSnippetsFile importPaths
    print snippetFiles
    case res of
        Left exc    -> throwM $ ModuleCompilationException exc
        Right units -> do
            let res = Map.fromList
                    $ map (\(a, b) -> (convert a, importsToHints b))
                    $ Map.toList units
            pure $ addSnippets res snippetFiles

