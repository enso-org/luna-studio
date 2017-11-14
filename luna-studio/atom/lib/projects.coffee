fse     = require 'fs-extra'
fs      = require 'fs-plus'
Git     = require 'nodegit'
path    = require 'path'
request = require 'request'
yaml    = require 'js-yaml'
InputView = require './input-view'

recentProjectsPath    = process.env.LUNA_STUDIO_DATA_PATH + '/recent-projects.yml'
defaultProjectPath    = process.env.LUNA_STUDIO_PROJECTS  or path.join(fs.getHomeDirectory(), 'projects')
temporaryPath         = process.env.LUNA_STUDIO_TEMP      or '/tmp'
tutorialsDownloadPath = process.env.LUNA_STUDIO_TUTORIALS or '/tmp/tutorials'
devMode               = process.env.LUNA_STUDIO_DEVELOP?

temporaryProject = {
    name: 'unsaved-luna-project',
    path: '/tmp/unsaved-luna-project',
    srcDir: 'src'
    mainFile: 'Main.luna'
    mainContent: 'def main:\n    None'
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
        fs.mkdir temporaryProject.path, (err) =>
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
                pane.destroyItem(paneItem)

openMainIfExists = ->
    projectPath = atom.project.getPaths()[0]
    return unless projectPath?
    mainLocation = projectPath + '/src/Main.luna'
    if fs.existsSync mainLocation
        atom.workspace.open(mainLocation, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})

isTemporary = (projectPath) -> projectPath.startsWith temporaryPath

selectLunaProject = (e) ->
    e.stopImmediatePropagation()
    atom.pickFolder openLunaProject

openLunaProject = (paths) ->
    if paths?
        closeAllFiles()
        atom.project.setPaths [paths[0]]
        openMainIfExists


module.exports =
    closeAllFiles: closeAllFiles
    openMainIfExists: openMainIfExists
    selectLunaProject: selectLunaProject
    openLunaProject: openLunaProject

    temporaryProject:
        path: temporaryProject.path
        open: (callback) =>
            closeAllFiles()
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
                    (name) =>!fs.existsSync(name),
                    (name) => "Path already exists at '#{name}'",
                    (name) => callback name
    recent:
        load: (callback) =>
            loadRecentNoCheck (recentProjectsPaths) =>
                recentProjectsPaths.forEach (recentProjectPath) =>
                    fs.access recentProjectPath, (err) =>
                        if not err
                            callback recentProjectPath

        add: (recentProjectPath) =>
            if isTemporary recentProjectPath then return
            loadRecentNoCheck (recentProjectsPaths) =>
                pos = recentProjectsPaths.indexOf(recentProjectPath);
                if pos != -1
                    recentProjectsPaths.splice(pos, 1)
                recentProjectsPaths.unshift(recentProjectPath)
                data = yaml.safeDump(recentProjectsPaths)
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
                atom.confirm
                    message: "Error while getting tutorials"
                    detailedMessage: error.message
                    buttons:
                        Ok: ->

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
            closeAllFiles()
            fse.remove dstPath, (err) =>
                clone = -> Git.Clone(tutorial.uri, dstPath, cloneOpts).then((repo) =>
                            atom.project.setPaths [dstPath]
                            finalize())


                clone().catch((error) =>
                    clone().catch(error) =>
                        atom.confirm
                            message: "Error while cloning tutorial"
                            detailedMessage: error.message
                            buttons:
                                Ok: ->
                        finalize())
