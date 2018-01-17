fse     = require 'fs-extra'
fs      = require 'fs-plus'
path    = require 'path'
request = require 'request'
yaml    = require 'js-yaml'
InputView = require './input-view'
report = require './report'
requestProgress = require 'request-progress'
unzip = require 'unzip'

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

mkRequestOpts = (url) ->
    url: url
    timeout: 10000
    headers:
        'User-Agent': 'luna-studio'

tutorialListRequestOpts = mkRequestOpts 'https://api.github.com/orgs/luna-packages/repos'
thumbnailRequestOpts = (name) -> mkRequestOpts 'https://api.github.com/repos/luna-packages/' + name + '/contents/thumb.png'

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
                request.get tutorialListRequestOpts, (err, response, body) =>
                    if err?
                        callback
                            error: 'Cannot download tutorial list. ' + err.message
                        return
                    parsed = yaml.safeLoad(body)
                    unless parsed.forEach?
                        parsed.message ?= ''
                        callback
                            error: 'Cannot download tutorial list. ' + parsed.message
                    else
                        parsed.forEach (repo) =>
                            archiveUrl = repo.archive_url.replace('{archive_format}', 'zipball').replace('{/ref}', '/master')
                            callback
                                name: repo.name
                                description: repo.description
                                uri: archiveUrl
                            request.get thumbnailRequestOpts(repo.name), (err, response, body) =>
                                if body?
                                    parsed = yaml.safeLoad(body)
                                    callback
                                        name: repo.name
                                        description: repo.description
                                        uri: archiveUrl
                                        thumb: 'data:image/png;base64,' + parsed.content
            catch error
                report.displayError 'Error while getting tutorials', error.message

        open: (tutorial, progress, finalize) ->
            dstPath = path.join tutorialsDownloadPath, tutorial.name
            dstZipPath = dstPath + '.zip'
            unpackPath = path.join tutorialsDownloadPath, 'unzipped' + tutorial.name
            cloneError = (err) =>
                report.displayError 'Error while cloning tutorial', err
                finalize()
            if closeAllFiles()
                fse.remove dstPath, (err) =>
                    if err?
                        cloneError err.toString()
                    else
                        requestProgress(request mkRequestOpts tutorial.uri)
                            .on 'progress', ((state) => progress state.percent)
                            .on 'error', cloneError
                            .pipe(unzip.Extract({ path: unpackPath }))
                            .on 'close', =>
                                fs.readdir unpackPath, (err, files) =>
                                    if err?
                                        cloneError 'Cannot open tutorial: ' + err.message
                                    else unless files[0]?
                                        cloneError 'Wrong tutorial archive structure'
                                    else
                                        srcPath = path.join unpackPath, files[0]
                                        fs.rename srcPath, dstPath, (err) =>
                                            if err?
                                                cloneError 'Cannot open tutorial: ' + err.message
                                            atom.project.setPaths [dstPath]
                                            finalize()
