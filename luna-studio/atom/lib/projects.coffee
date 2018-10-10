fse     = require 'fs-extra'
fs      = require 'fs-plus'
fsNoEPERMMod = require 'fs-no-eperm-anymore'
fsNoEPERM = fsNoEPERMMod.instantiate()
path    = require 'path'
request = require 'request'
yaml    = require 'js-yaml'
InputView = require './input-view'
report = require './report'
requestProgress = require 'request-progress'
unzip = require 'unzipper'

{ProjectItem, recentClasses, sampleProjectClasses} = require './project-item'

recentProjectsPath    = path.join process.env.LUNA_STUDIO_DATA_PATH, 'recent-projects.yml'
sampleProjectsBackupPath   = path.join process.env.LUNA_STUDIO_DATA_PATH, 'sampleProjects-backup.yml'
defaultProjectPath    = process.env.LUNA_PROJECTS
temporaryPath         = process.env.LUNA_TMP
sampleProjectsDownloadPath = process.env.LUNA_TUTORIALS

temporaryProjectPath = path.join temporaryPath, 'UnsavedLunaProject'

encoding = 'utf8'

mkRequestOpts = (url) ->
    url: url
    timeout: 10000
    headers:
        'User-Agent': 'luna-studio'

## TUTORIALS ##

sampleProjectListRequestOpts = mkRequestOpts 'https://api.github.com/orgs/luna-packages/repos'
thumbnailRequestOpts = (name) -> mkRequestOpts 'https://api.github.com/repos/luna-packages/' + name + '/contents/thumb.png'
sampleProjectItems = {}

refreshSampleProjectList = (callback) =>
    onError = (errMessage) =>
        sampleProjectListDeserialize (error) =>
            if error?
                if errMessage?
                    errMessage = ', details: ' + errMessage
                callback 'Cannot download sample project list. \n\n Could not reach Github' + errMessage
            else
                callback()
    try
        request.get sampleProjectListRequestOpts, (err, response, body) =>
            if err?
                onError err.message
                return
            parsed = yaml.safeLoad(body)
            unless parsed.forEach?
                parsed.message ?= ''
                onError parsed.message
            else
                parsed.forEach (repo) =>
                    archiveUrl = repo.archive_url.replace('{archive_format}', 'zipball').replace('{/ref}', '/master')
                    sampleProjectItems[repo.name] =
                        name: repo.name
                        description: repo.description
                        uri: archiveUrl
                    callback()
                    sampleProjectListSerialize()
                    request.get thumbnailRequestOpts(repo.name), (err, response, body) =>
                        if body?
                            parsed = yaml.safeLoad(body)
                            sampleProjectItems[repo.name] =
                                name: repo.name
                                description: repo.description
                                uri: archiveUrl
                                thumb: 'data:image/png;base64,' + parsed.content
                            callback()
                            sampleProjectListSerialize()
    catch error
        report.displayError 'Error while getting sampleProjects. Could not reach Github. ', error.message

sampleProjectListSerialize = =>
    data = yaml.safeDump sampleProjectItems
    fs.writeFile sampleProjectsBackupPath, data, encoding, (err) =>
        if err?
            report.silentError err

sampleProjectListDeserialize = (callback) =>
    fs.readFile sampleProjectsBackupPath, encoding, (err, data) =>
        if err? then callback
                        error: err.message
        else
            sampleProjectItems = yaml.safeLoad(data)
            callback()

mkSampleProject = (sampleProject) ->
    new ProjectItem sampleProject, sampleProjectClasses, (progress, finalize) =>
        sampleProjectOpen sampleProject, progress, finalize

retryEBUSY = (operation, callback) ->
    retryCount = 0
    maxRetries = 8
    waitTimeMS = 100
    retry = => operation (err) =>
        if err? and (err.code == 'EBUSY')
            if retryCount >= maxRetries
                callback err
            else
                waitTimeMS *= 2
                retryCount++
                console.warn 'resource EBUSY, retry ', retryCount, 'wait', waitTimeMS, 'ms'
                setTimeout retry, waitTimeMS
        else
            callback err
    retry()

sampleProjectOpen = (sampleProject, progress, finalize) ->
    dstPath = path.join sampleProjectsDownloadPath, sampleProject.name
    dstZipPath = dstPath + '.zip'
    unpackPath = path.join sampleProjectsDownloadPath, 'unzipped' + sampleProject.name
    cloneError = (err) =>
        report.displayError 'Error while cloning sample project', err
        finalize()
    tryCloseAllFiles =>
        retryEBUSY ((callback) => fse.remove dstPath, callback), (err) =>
            if err?
                cloneError err.toString()
            else
                requestProgress(request mkRequestOpts sampleProject.uri)
                    .on 'progress', ((state) => progress state.percent)
                    .on 'error', cloneError
                    .pipe(unzip.Extract({ path: unpackPath }))
                    .on 'close', =>
                        fs.readdir unpackPath, (err, files) =>
                            if err?
                                cloneError 'Cannot open sample project: ' + err.message
                            else unless files[0]?
                                cloneError 'Wrong sample project archive structure'
                            else
                                srcPath = path.join unpackPath, files[0]
                                fsNoEPERM.rename srcPath, dstPath
                                    .then =>
                                        atom.project.setPaths [dstPath]
                                        finalize()
                                    .catch (err) =>
                                        cloneError 'Cannot open sample project: ' + err.message

## RECENT PROJECTS ##

recentProjects = []
recentProjectsPaths = ->
    paths = []
    for recentProject in recentProjects
        paths.push recentProject.uri
    return paths

mkRecentProject = (projectPath) ->
    new ProjectItem {uri: projectPath}, recentClasses, (progress, finalize) =>
        progress 0.5
        tryCloseAllFiles =>
            atom.project.setPaths [projectPath]
        finalize()

loadRecentNoCheck = (callback) =>
    fs.readFile recentProjectsPath, encoding, (err, data) =>
        projectsPaths = []
        if err?
            console.log err
        else
            parsed = yaml.safeLoad(data)
            if parsed?
                projectsPaths = parsed
        callback projectsPaths

## TEMPORARY PROJECT ##

isTemporary = (projectPath) -> (projectPath.startsWith temporaryPath) or (projectPath.startsWith sampleProjectsDownloadPath)

## PROJECTS ##

closingAll = false

tryCloseAllFiles = (callback) ->
    closingAll = true
    x = atom.project.onDidChangePaths =>
        for pane in atom.workspace.getPanes()
            for paneItem in pane.getItems()
                if atom.workspace.isTextEditor(paneItem) or paneItem.isLunaCodeEditorTab
                    unless pane.destroyItem paneItem
                        x.dispose()
                        closingAll = false
                        return
        x.dispose()
        callback()
        closingAll = false
    atom.project.setPaths []

openMainIfExists = ->
    projectPath = atom.project.getPaths()[0]
    return unless projectPath?
    mainLocation = path.join projectPath, 'src', 'Main.luna'
    if fs.existsSync mainLocation
        atom.workspace.open(mainLocation, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})
        target = atom.views.getView atom.workspace
        atom.commands.dispatch(target, 'tree-view:reveal-active-file')


selectLunaProject = (e) ->
    e.stopImmediatePropagation()
    atom.pickFolder openLunaProject

openLunaProject = (paths) ->
    if paths?
        tryCloseAllFiles =>
            atom.project.setPaths [paths[0]]
            openMainIfExists

## EXPORTS ##

module.exports =
    class ProjectManager
        constructor: (@codeEditor) ->
        openMainIfExists: openMainIfExists
        selectLunaProject: selectLunaProject
        openLunaProject: openLunaProject
        isClosingAll: => closingAll
        createProject: =>
            tryCloseAllFiles =>
                fse.remove temporaryProjectPath, (err) =>
                    @codeEditor.pushInternalEvent
                        tag: "CreateProject"
                        _path: temporaryProjectPath

        temporaryProjectSave: (callback) =>
                if isTemporary atom.project.getPaths()[0]
                    inputView = new InputView()
                    suggestedProjectName = path.basename(atom.project.getPaths()[0])
                    inputView.attach "Save project as", defaultProjectPath, suggestedProjectName,
                        (name) => !fs.existsSync(name),
                        (name) => "Path already exists at '#{name}'",
                        (name) => callback name
        getRecentItems: -> recentProjects

        refreshRecentList: (callback) =>
            recentProjects = []
            loadRecentNoCheck (serializedProjectPaths) =>
                serializedProjectPaths.forEach (serializedProjectPath) =>
                    try
                        fs.accessSync serializedProjectPath
                        recentProjects.push mkRecentProject serializedProjectPath
                    catch error # we can just silently omit non-existing projects
                callback?()

        addRecent: (recentProjectPath) =>
            return if isTemporary recentProjectPath
            recentProjects = recentProjects.filter (project) -> project.uri isnt recentProjectPath
            recentProjects.unshift mkRecentProject recentProjectPath
            data = yaml.safeDump recentProjectsPaths()
            fs.writeFile recentProjectsPath, data, encoding, (err) =>
                if err?
                    console.log err

        getSampleProjectItems: =>
            sampleProjects = {}
            for own key, sampleProjectItem of sampleProjectItems
                sampleProjects[key] = mkSampleProject sampleProjectItem
            sampleProjects

        refreshSampleProjectList: refreshSampleProjectList
