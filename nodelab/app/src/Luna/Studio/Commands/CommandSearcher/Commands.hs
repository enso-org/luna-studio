{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Luna.Studio.Commands.CommandSearcher.Commands
    ( querySearchCmd
    , queryTreeCmd
    , runCommand
    , help
    ) where

import qualified Data.Map                         as Map
import           Data.Text.Lazy                   (stripPrefix)
import qualified Data.Text.Lazy                   as Text
import           Luna.Studio.Prelude                hiding (Item, stripPrefix)
import           Luna.Studio.Data.Vector

import qualified Luna.Studio.Batch.Workspace                  as Workspace
import qualified Empire.API.Data.Project          as Project
import           Event.Event                      (JSState)
import qualified JS.GoogleAnalytics               as GA
import qualified JS.NodeSearcher                  as UI
import qualified Luna.Studio.Commands.Batch          as BatchCmd
import qualified Luna.Studio.Commands.CodeEditor     as CodeEditor
import           Luna.Studio.Commands.Command        (Command, performIO)
import           Luna.Studio.Commands.NodeSearcher   as NS
import           Luna.Studio.Commands.ProjectManager (loadProject)
import qualified Luna.Studio.State.Global            as Global
import           Text.ScopeSearcher.Item          (Item (..))
import qualified Text.ScopeSearcher.Scope         as Scope


commands :: Command Global.State ([(Text, Item)])
commands = do
    projects <- uses (Global.workspace . Workspace.projects) Map.elems
    gaState  <- uses Global.jsState gaEnabled
    let projectToItem p = (name, Element) where name = Text.pack $ p ^. Project.name
        projectList = Map.fromList $ projectToItem <$> projects
        projectCmd  = Map.fromList [ ("new",    Element)
                                   , ("export", Element)
                                   , ("open",   Group projectList)
                                   -- , ("rename", Element)
                                   ]
        settingsCmd = Map.fromList [gaElement]
        gaElement   = if gaState then ("disableGoogleAnalytics", Element)
                                 else ("enableGoogleAnalytics", Element)
    return [ ("project",          Group projectCmd)
           , ("insert",           Element)
           , ("help",             Element)
           , ("toggleTextEditor", Element)
           , ("settings",         Group settingsCmd)
           ]


createProject :: Text -> Command Global.State ()
createProject name = BatchCmd.createProject name

openProject :: Text -> Command Global.State ()
openProject name = do
    projs <- use $ Global.workspace . Workspace.projects
    let mayProject = find (\(_,p) -> p ^. Project.name == (Text.unpack name)) (Map.toList projs)
    case mayProject of
        Just (projectId, _) -> loadProject projectId
        Nothing             -> performIO $ putStrLn "Project not found"



help :: Command Global.State ()
help = do
    GA.sendEvent $ GA.OpenHelp
    performIO $ openHelp'


foreign import javascript safe "$('.tutorial-box').show().focus()"    openHelp'  :: IO ()
foreign import javascript safe "common.enableGA($1)"    enableGA' :: Bool -> IO ()
foreign import javascript safe "$1.isGAEnabled()"       gaEnabled :: JSState -> Bool

enableGA :: Bool -> Command a ()
enableGA val = do
    GA.sendEvent $ GA.GAOptOut val
    performIO $ enableGA' val


runCommand :: Text -> Command Global.State ()
runCommand "project.new"                              = performIO $ UI.initNodeSearcher "project.new untitled" Nothing (Vector2 200 200) True
runCommand (stripPrefix "project.new "  -> Just name) = createProject name
runCommand (stripPrefix "project.open." -> Just name) = openProject name
runCommand "project.export"                           = exportCurrentProject
runCommand "help"                                     = help
runCommand "insert"                                   = NS.openFresh
runCommand "toggleTextEditor"                         = CodeEditor.toggle
runCommand "settings.disableGoogleAnalytics"          = enableGA False
runCommand "settings.enableGoogleAnalytics"           = enableGA True
runCommand cmd                                        = performIO $ putStrLn $ "Unknown command " <> (Text.unpack cmd)

querySearchCmd :: Text -> Command Global.State ()
querySearchCmd query = do
    sd <- commands
    let sd'   = Map.fromList sd
        items = Scope.searchInScope sd' query
    performIO $ UI.displayQueryResults UI.CommandSearcher items

queryTreeCmd :: Text -> Command Global.State ()
queryTreeCmd query = do
    sd <- commands
    let sd'   = Map.fromList sd
        items = Scope.moduleItems sd' query
    performIO $ UI.displayTreeResults UI.CommandSearcher items


exportCurrentProject :: Command Global.State ()
exportCurrentProject = (use $ Global.workspace . Workspace.currentProjectId) >>= BatchCmd.exportProject
