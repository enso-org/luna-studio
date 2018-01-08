fse     = require 'fs-extra'
fs      = require 'fs-plus'
Git     = require 'nodegit'
path    = require 'path'
request = require 'request'
yaml    = require 'js-yaml'
InputView = require './input-view'
report = require './report'
{ProjectItem, recentClasses} = require './project-item'

recentProjectsPath    = path.join process.env.LUNA_STUDIO_DATA_PATH, 'recent-projects.yml'
defaultProjectPath    = process.env.LUNA_PROJECTS
temporaryPath         = process.env.LUNA_TMP
tutorialsDownloadPath = process.env.LUNA_TUTORIALS

temporaryProject = {
    name: 'unsaved-luna-project'
    path: path.join temporaryPath, 'unsaved-luna-project'
    srcDir: 'src'
    mainFile: 'Main.luna'
    mainContent: 'def main:\n    hello = "Hello, World!"\n    None'
    }

temporaryMainFilePath = path.join temporaryProject.path, temporaryProject.srcDir, temporaryProject.mainFile

encoding = 'utf8'

tokenUser = 'luna-studio'
token = '01665947e42b84759406bc56a72ec141575653d1'
accessToken = '?access_token=' + token

tutorialRequestOpts =
    url: 'https://api.github.com/orgs/luna-packages/repos' + accessToken
    headers:
        'User-Agent': 'luna-studio'

thumbnailRequestOpts = (name) ->
    url: 'https://api.github.com/repos/luna-packages/' + name + '/contents/thumb.png' + accessToken
    headers:
        'User-Agent': 'luna-studio'

loadRecentNoCheck = (callback) =>
    fs.readFile recentProjectsPath, encoding, (err, data) =>
        recentProjectsPaths = []
        if err
            console.log err
        else
            parsed = yaml.safeLoad(data)
            if parsed?
                recentProjectsPaths = parsed
        callback recentProjectsPaths

createTemporary = (callback) =>
    fse.remove temporaryProject.path, (err) =>
        fse.mkdirs temporaryProject.path, (err) =>
            if err then throw err
            srcPath = path.join temporaryProject.path, temporaryProject.srcDir
            fs.mkdir srcPath, (err) =>
                if err then throw err
                mainPath = path.join srcPath, temporaryProject.mainFile
                fs.writeFile mainPath, temporaryProject.mainContent, (err) =>
                    if err then throw err
                    projectPath = path.join temporaryProject.path, '.lunaproject'
                    fs.writeFile projectPath, '', (err) =>
                        if err then throw err
                        callback()

closeAllFiles = ->
    for pane in atom.workspace.getPanes()
        for paneItem in pane.getItems()
            if atom.workspace.isTextEditor(paneItem) or paneItem.isLunaCodeEditorTab
                unless pane.destroyItem paneItem
                    return false
    return true

openMainIfExists = ->
    projectPath = atom.project.getPaths()[0]
    return unless projectPath?
    mainLocation = path.join projectPath, 'src', 'Main.luna'
    if fs.existsSync mainLocation
        atom.workspace.open(mainLocation, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})

isTemporary = (projectPath) -> (projectPath.startsWith temporaryPath) or (projectPath.startsWith tutorialsDownloadPath)

selectLunaProject = (e) ->
    e.stopImmediatePropagation()
    atom.pickFolder openLunaProject

openLunaProject = (paths) ->
    if paths?
        if closeAllFiles()
            atom.project.setPaths [paths[0]]
            openMainIfExists

recentProjects = []
recentProjectsPaths = ->
    paths = []
    for recentProject in recentProjects
        paths.push recentProject.uri
    return paths

mkRecentProject = (projectPath) ->
    new ProjectItem {uri: projectPath}, recentClasses, (progress, finalize) =>
        progress 0.5
        if closeAllFiles()
            atom.project.setPaths [projectPath]
        finalize()

module.exports =
    closeAllFiles: closeAllFiles
    openMainIfExists: openMainIfExists
    selectLunaProject: selectLunaProject
    openLunaProject: openLunaProject

    temporaryProject:
        path: temporaryProject.path
        open: (callback) =>
            if closeAllFiles()
                createTemporary =>
                    atom.project.setPaths [temporaryProject.path]
                    callback?()
        isOpen: =>
            return isTemporary atom.project.getPaths()[0]

        save: (callback) =>
            if isTemporary atom.project.getPaths()[0]
                inputView = new InputView()
                suggestedProjectName = path.basename(atom.project.getPaths()[0])
                inputView.attach "Save project as", defaultProjectPath, suggestedProjectName,
                    (name) => !fs.existsSync(name),
                    (name) => "Path already exists at '#{name}'",
                    (name) => callback name
    recent:
        getItems: -> recentProjects

        refreshProjectsList: (callback) =>
            recentProjects = []
            loadRecentNoCheck (serializedProjectPaths) =>
                serializedProjectPaths.forEach (serializedProjectPath) =>
                    try
                        fs.accessSync serializedProjectPath
                        recentProjects.push mkRecentProject serializedProjectPath
                    catch error
                callback?()

        add: (recentProjectPath) =>
            if isTemporary recentProjectPath then return
            recentProjects = recentProjects.filter (project) -> project.uri isnt recentProjectPath
            recentProjects.unshift mkRecentProject recentProjectPath
            data = yaml.safeDump recentProjectsPaths()
            fs.writeFile recentProjectsPath, data, encoding, (err) =>
                if err?
                    console.log err
    tutorial:
        list: (callback) =>
            try
                request.get tutorialRequestOpts, (err, response, body) =>
                    parsed = yaml.safeLoad(body)
                    if body?
                        parsed.forEach (repo) =>
                            callback
                                name: repo.name
                                description: repo.description
                                uri: repo.html_url
                            request.get thumbnailRequestOpts(repo.name), (err, response, body) =>
                                if body?
                                    parsed = yaml.safeLoad(body)
                                    callback
                                        name: repo.name
                                        description: repo.description
                                        uri: repo.html_url
                                        thumb: 'data:image/png;base64,' + parsed.content
                    else
                        callback
                            error: 'Cannot download tutorial list.'
            catch error
                report.displayError 'Error while getting tutorials', error.message

        open: (tutorial, progress, finalize) ->
            dstPath = tutorialsDownloadPath + '/' + tutorial.name
            cloneAttempts = 0
            cloneOpts =
                fetchOpts:
                    callbacks:
                        certificateCheck: => 1
                        credentials: (url, userName) =>
                            if cloneAttempts > 0
                                return Git.Cred.sshKeyFromAgent(userName)
                            cloneAttempts++
                            return Git.Cred.userpassPlaintextNew(tokenUser, token)
                        transferProgress: (stats) =>
                            p = (stats.receivedObjects() + stats.indexedObjects()) / (stats.totalObjects() * 2)
                            try
                                progress p
                            catch error
                                console.log error
            clone = -> Git.Clone(tutorial.uri, dstPath, cloneOpts).then (repo) =>
                atom.project.setPaths [dstPath]
                finalize()
            cloneError = (err) =>
                report.displayError 'Error while cloning tutorial', err
                finalize()
            if closeAllFiles()
                fse.remove dstPath, (err) =>
                    if err?
                        cloneError err.toString()
                    else
                        clone().catch (error) =>
                            clone().catch (error) =>
                                cloneError error
