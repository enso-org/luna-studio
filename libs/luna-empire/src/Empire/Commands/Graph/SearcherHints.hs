module Empire.Commands.Graph.SearcherHints where

import Prologue

import qualified Data.Bimap                            as Bimap
import qualified Data.Map                              as Map
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
import qualified LunaStudio.Data.Searcher.Hint.Class   as Hint
import qualified LunaStudio.Data.Searcher.Hint.Library as SearcherLibrary
import qualified Path

import Control.Monad.Catch           (try)
import Empire.ASTOp                  (liftScheduler)
import Empire.Data.AST               (astExceptionFromException,
                                      astExceptionToException)
import Empire.Empire                 (Empire)
import Luna.Pass.Data.Stage          (Stage)
import LunaStudio.Data.GraphLocation (GraphLocation)
import Luna.Package        (PackageNotFoundException)
import Control.Monad.Exception (MonadException)

data ModuleCompilationException
    = ModuleCompilationException ModLoader.UnitLoadingError
    deriving (Show)

instance Exception ModuleCompilationException where
    toException   = astExceptionToException
    fromException = astExceptionFromException

getImportPaths :: (MonadThrow m, MonadIO m, Monad) => GraphLocation -> m [FilePath]
getImportPaths location = do
    let file = location ^. GraphLocation.filePath
    currentProjPath <- Package.packageRootForFile =<< Path.parseAbsFile file
    importPaths     <- Package.packageImportPaths currentProjPath
    pure $ snd <$> importPaths

isPublicMethod :: IR.Name -> Bool
isPublicMethod (convert -> n) = head n /= Just '_'

importsToHints :: Unit.Unit -> SearcherLibrary.Library
importsToHints (Unit.Unit definitions classes) = let
    funToHint (n,d) = Hint.Raw 
        (convert n) 
        $ fromMaybe mempty $ d ^. Def.documentation
    funHints   = funToHint <$> Map.toList (unwrap definitions)
    classHints = (classToHints . view Def.documented) <$> classes
    in SearcherLibrary.Library funHints (Map.mapKeys convert classHints) []

classToHints :: Class.Class -> Hint.Class
classToHints (Class.Class constructors methods _) = let
    getDocumentation    = fromMaybe mempty . view Def.documentation
    constructorsNames   = Map.keys constructors
    constructorToHint   = flip Hint.Raw mempty . convert
    constructorsHints   = constructorToHint <$> constructorsNames
    methods'            = filter (isPublicMethod . fst)
                        . Map.toList $ unwrap methods
    methodToHint (n, d) = Hint.Raw (convert n) $ getDocumentation d
    methodsHints        = methodToHint <$> methods'
    in Hint.Class constructorsHints methodsHints []

getSearcherHints :: GraphLocation -> Empire SearcherLibrary.Set
getSearcherHints loc = do
    importPaths     <- liftIO $ getImportPaths loc
    availableSource <- liftIO $ for importPaths $ \path -> do
        sources <- Package.findPackageSources =<< Path.parseAbsDir path
        return $ Bimap.toMapR sources
    let union = Map.map (Path.toFilePath) $ Map.unions availableSource
    -- importsMVar <- view modules
    -- cmpModules  <- liftIO $ readMVar importsMVar
    res         <- try $ liftScheduler $ do
        ModLoader.init
        stdUnitRef <- snd <$> Std.stdlib @Stage
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr @Unit.UnitRefsMap $ wrap $ Map.singleton "Std.Primitive" stdUnitRef
        for (Map.keys union) $ ModLoader.loadUnit def union []
        refsMap <- Scheduler.getAttr @Unit.UnitRefsMap
        units <- flip Map.traverseWithKey (unwrap refsMap) $ \name unitRef -> case unitRef ^. Unit.root of
            Unit.Graph termUnit   -> UnitMapper.mapUnit name termUnit
            Unit.Precompiled unit -> return unit
        return units
    case res of
        Left exc    -> throwM $ ModuleCompilationException exc
        Right units -> do
            let res = Map.fromList
                    $ map (\(a, b) -> (convert a, importsToHints b))
                    $ Map.toList units
            pure $ res & ix "Std.HTTP" . SearcherLibrary.snippets .~ [ Hint.Raw "Http.getJSON \"http://ip.jsontest.org\"" "Fetch data from a JSON endpoint" ]

