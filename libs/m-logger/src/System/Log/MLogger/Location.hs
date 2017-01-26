module System.Log.MLogger.Location where

import           Language.Haskell.TH.Syntax
import           Prologue



loc :: Q Exp
loc = do
    Loc _ _ modName start _ <- location
    let (line, pos) = start
    return $ TupE [ LitE (StringL modName)
                  , LitE (IntegerL $ toInteger line)
                  , LitE (IntegerL $ toInteger pos)
                  ]

moduleName :: Q Exp
moduleName = do
    Loc _ _ modName _ _ <- location
    return $ LitE (StringL modName)
