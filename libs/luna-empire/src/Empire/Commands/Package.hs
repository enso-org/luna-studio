module Empire.Commands.Package where

import Prologue

import           LunaStudio.Data.Error           (Error, LunaError)
import qualified LunaStudio.Data.Error           as Luna
import           Luna.Package.Structure.Generate (GeneratorError)
import qualified Luna.Package.Structure.Generate as Generator


prepareLunaError :: GeneratorError -> Error LunaError
prepareLunaError = \case
    Generator.InvalidPackageLocation msg ->
        Luna.Error (Luna.PackageGenerator Luna.InvalidPackageLocation) msg
    Generator.InvalidPackageName     msg ->
        Luna.Error (Luna.PackageGenerator Luna.InvalidPackageName)     msg
    Generator.SystemError            msg ->
        Luna.Error (Luna.PackageGenerator Luna.SystemError)            msg
