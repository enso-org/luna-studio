module LunaStudio.API.AsyncUpdate where

import qualified LunaStudio.API.Atom.GetBuffer              as GetBuffer
import qualified LunaStudio.API.Atom.Substitute             as Substitute
import qualified LunaStudio.API.Graph.MonadsUpdate          as MonadsUpdate
import qualified LunaStudio.API.Graph.NodeResultUpdate      as NodeResult
import qualified LunaStudio.API.Graph.NodeTypecheckerUpdate as NodeTCUpdate
import           Prologue


data AsyncUpdate = MonadsUpdate      MonadsUpdate.Update
                 | TypecheckerUpdate NodeTCUpdate.Update
                 | ResultUpdate        NodeResult.Update
                 | CodeUpdate          Substitute.Update
                 | LexerUpdate          GetBuffer.LexUpdate
                 deriving (Eq, Show)
