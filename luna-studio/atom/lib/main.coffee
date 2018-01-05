
fs       = require 'fs-plus'
path     = require 'path'
yaml     = require 'js-yaml'

VisualGuide      = require './guide'
stats = require './stats'
analytics = require './gen/analytics'
LunaCodeEditorTab  = require './luna-code-editor-tab'
LunaNodeEditorTab  = require './luna-node-editor-tab'
LunaWelcomeTab = require './luna-welcome-tab'
LunaToolbar = require './luna-toolbar'
LunaSemanticGrammar = require './luna-grammar'
projects  = require './projects'
Statusbar = require './statusbar-view'
(require './luna-visualizers')()
codeEditor = (require './gen/text-editor-ghcjs.js')()
nodeEditor = (require './gen/node-editor-ghcjs.js')()


LUNA_STUDIO_URI  = 'atom://luna/studio'

analyticsConfigRequest =
    url: 'https://raw.githubusercontent.com/luna/luna-studio-config/master/analytics.yml'
    headers:
        'User-Agent': 'luna-studio'

module.exports = LunaStudio =
    activate: (state) ->
        stats.initialize()
        atom.grammars.addGrammar(new LunaSemanticGrammar(atom.grammars, codeEditor.lex))
        atom.workspace.addOpener @lunaOpener
        codeEditor.connect(nodeEditor.connector)
        @welcome = new LunaWelcomeTab(codeEditor)
        @toolbar = new LunaToolbar(codeEditor)
        @guide   = new VisualGuide(nodeEditor)
        @moving = false

        actStatus = (act, arg1, arg2) =>
            switch act
                when 'Init'
                    rootPath = atom.project.getPaths().shift()
                    if rootPath? and rootPath != ""
                        projects.recent.add rootPath
                        codeEditor.pushInternalEvent(tag: "SetProject", _path: rootPath)
                when 'ProjectSet'
                    projects.openMainIfExists()
                when 'FileOpened'
                    codeEditor.pushInternalEvent(tag: "GetBuffer", _path: arg1)
                when 'ProjectMove'
                    moveUri = (oldUri) -> if oldUri? and oldUri.startsWith arg2
                        return arg1 + oldUri.slice arg2.length
                    @moving = true
                    atom.project.setPaths [arg1]
                    for pane in atom.workspace.getPaneItems()
                        if pane instanceof LunaCodeEditorTab
                            newUri = moveUri pane.uri
                            pane.setUri newUri if newUri?
                        else if pane instanceof LunaNodeEditorTab
                            newUri = moveUri pane.uri
                            if newUri?
                                pane.uri = newUri
                                nodeEditor.pushEvent(tag: "UpdateFilePath", path: newUri)

        codeEditor.onStatus actStatus
        atom.workspace.onDidChangeActivePaneItem (item) => @handleItemChange(item)
        atom.workspace.onDidDestroyPaneItem (event) => @handleItemDestroy(event)
        atom.workspace.observeTextEditors (editor) => @handleSaveAsLuna(editor)
        atom.workspace.onDidAddPaneItem (pane)   => @handleItemChange(pane.item)
        atom.project.onDidChangePaths (projectPaths) => @handleProjectPathsChange(projectPaths)
        atom.workspace.open(LUNA_STUDIO_URI, {split: atom.config.get('luna-studio.preferredNodeEditorPosition')})
        atom.commands.add 'atom-workspace',
            'application:add-project-folder': projects.selectLunaProject
            'application:open':               projects.selectLunaProject
            'application:open-folder':        projects.selectLunaProject
        atom.commands.add 'body',
            'luna-studio:welcome': => @welcome.attach()
            'luna-studio:guide':   => @guide.start()
            'core:cancel': => @welcome.cancel()
        atom.packages.onDidActivateInitialPackages =>
            @toolbar.attach()
            atom.reopenProjectMenuManager.open = projects.openLunaProject
            openTemporaryProject = => projects.temporaryProject.open (err) => if err then throw err
            resetProjects = atom.config.get('luna-studio.resetProjects') and atom.project.getPaths().length == 0
            if atom.config.get('luna-studio.showWelcomeScreen') and atom.project.getPaths().length == 0
                @welcome.attach()
                if resetProjects
                    @welcome.onCancel = openTemporaryProject
            else if resetProjects
                openTemporaryProject()
            if atom.config.get('luna-studio.showWelcomeGuide')
                @guide.start()
        codeEditor.start()



    consumeStatusBar: (statusBar) ->
        myElement = new Statusbar(codeEditor)
        @statusBarTile = statusBar.addLeftTile(item: myElement, priority: -1)

    deserializeLunaCodeEditorTab: ({uri}) ->
        actStatus = (status) ->
            if status == 'Init'
                atom.workspace.open(uri, {split: atom.config.get('luna-studio.preferredCodeEditorPosition')})

        codeEditor.statusListener actStatus

    lunaOpener: (uri) ->
        if uri is LUNA_STUDIO_URI
            new LunaNodeEditorTab null, nodeEditor, codeEditor
        else if path.extname(uri) is '.luna'
            new LunaCodeEditorTab uri, codeEditor

    deactivate: ->
        stats.finalize()
        @statusBarTile?.destroy()
        @statusBarTile = null

    setNodeEditorUri: (uri) ->
        nodeEditorTab = @getNodeEditorTab()
        nodeEditorTab.uri = uri if nodeEditorTab?
        if uri?
            nodeEditor.pushEvent(tag: "SetFile", path: uri)
        else
            nodeEditor.pushEvent(tag: "UnsetFile")

    getNodeEditorTab: =>
        for i in atom.workspace.getPaneItems()
            return i if i instanceof LunaNodeEditorTab

    handleItemChange: (item) ->
        if item instanceof LunaCodeEditorTab
            @setNodeEditorUri item.uri

    handleItemDestroy: (event) =>
        if (event.item instanceof LunaCodeEditorTab)
            urisOf = (instance) ->
                pane.uri for pane in atom.workspace.getPaneItems().filter((a) -> a instanceof instance)
            codeUris  = urisOf LunaCodeEditorTab
            graphUris = urisOf LunaNodeEditorTab
            if event.item.uri not in codeUris #last opened file
                if event.item.uri in graphUris
                    nodeEditor.pushEvent(tag: "UnsetFile")
                return codeEditor.pushInternalEvent(tag: "CloseFile", _path: event.item.uri)

    handleSaveAsLuna: (editor) ->
        editor.getSaveDialogOptions = ->
            projectPath = atom.project.getPaths()[0]
            unless projectPath? then return {}
            srcPath = projectPath + '/src'
            unless fs.isDirectorySync srcPath
                srcPath = projectPath
            return { defaultPath: srcPath }
        editor.onDidSave (e) =>
            if path.extname(e.path) is ".luna" and not (editor instanceof LunaCodeEditorTab)
                atom.workspace.destroyActivePaneItem()
                atom.workspace.open(e.path)

    handleProjectPathsChange: (projectPaths) ->
        projectPath = projectPaths[0]
        if projectPath?
            projects.recent.add projectPath
            codeEditor.pushInternalEvent(tag: "SetProject", _path: projectPath)
            analytics.track 'LunaStudio.Project.Open',
                name: path.basename projectPath
                path: projectPath
            @welcome.close()
        if @moving
            @moving = false
        else
            @setNodeEditorUri null
        @guide.startProject()

    config:
        showWelcomeScreen:
            title: 'Welcome screen'
            description: 'Show welcome screen on start up'
            type: 'boolean'
            default: true
        showWelcomeGuide:
            title: 'Welcome guide'
            description: 'Show welcome guide on start up'
            type: 'boolean'
            default: true
        preferredNodeEditorPosition:
            title: 'Preferred pane for node editor'
            type: 'string'
            default: 'up'
            enum: [
                { value: 'left' , description: 'Left pane' }
                { value: 'right', description: 'Right pane' }
                { value: 'up'   , description: 'Upper pane' }
                { value: 'down' , description: 'Lower pane' }
            ]
        preferredCodeEditorPosition:
            title: 'Preferred pane for code editor'
            type: 'string'
            default: 'down'
            enum: [
                { value: 'left' , description: 'Left pane' }
                { value: 'right', description: 'Right pane' }
                { value: 'up'   , description: 'Upper pane' }
                { value: 'down' , description: 'Lower pane' }
            ]
        typecolors_l:
            title: 'Set L for LCH type colouring'
            type: 'number'
            default: 30

        typecolors_c:
            title: 'Set C for LCH type colouring'
            type: 'number'
            default: 45

        typecolors_h:
            title: 'Set initial H for LCH type colouring'
            type: 'number'
            default: 100.7

        analyticsEnabled:
            title: 'Send anonymous data to improve the application'
            type: 'boolean'
            default: true

        resetProjects:
            title: 'Open empty project on start up'
            type: 'boolean'
            default: true
