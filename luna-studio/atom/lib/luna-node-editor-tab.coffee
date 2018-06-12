path = require 'path'
{View} = require 'atom-space-pen-views'
projects = require './projects'
nodeEditorBaseGL = require 'node_editor_basegl'

uniqueTabNo = 0

module.exports =
class LunaNodeEditorTab extends View
    mountPoint = ""
    mountPoint2 = "luna-mount-basegl"
    pushShortcutEvent = null
    pushSearcherEvent = null

    constructor: (@uri, @nodeEditor, @codeEditor) ->
        super
        @on 'contextmenu', -> false
        @handleEvents()
        pushShortcutEvent = (name, arg = null) => @nodeEditor.pushEvent({_shortcut: name, _arg : arg})
        pushSearcherEvent = (name, arg = null) => @nodeEditor.pushEvent(if arg == null then {tag: name} else {tag: name, contents : arg})
        @nodeEditor.start @uri, mountPoint
        window.install = => nodeEditorBaseGL.install mountPoint2, 'atom://luna-studio/rsc/',  (ne) =>
            window.nodeEditor = ne
            @nodeEditor.setView ne
        nodeEditorBaseGL.onEvent (path, event, target) =>
            if event.tag.endsWith "Event"
                evt = if event.tag != 'MouseEvent' then event else
                    tag: event.constructor.name
                    altKey: event.altKey
                    bubbles: event.bubbles
                    button: event.button
                    buttons: event.buttons
                    cancelBubble: event.cancelBubble
                    cancelable: event.cancelable
                    clientX: event.clientX
                    clientY: event.clientY
                    ctrlKey: event.ctrlKey
                    defaultPrevented: event.defaultPrevented
                    detail: event.detail
                    eventPhase: event.eventPhase
                    isTrusted: event.isTrusted
                    layerX: event.layerX
                    layerY: event.layerY
                    metaKey: event.metaKey
                    movementX: event.movementX
                    movementY: event.movementY
                    offsetX: event.offsetX
                    offsetY: event.offsetY
                    pageX: event.pageX
                    pageY: event.pageY
                    returnValue: event.returnValue
                    screenX: event.screenX
                    screenY: event.screenY
                    shiftKey: event.shiftKey
                    sourceCapabilities: event.sourceCapabilities
                    timeStamp: event.timeStamp
                    type_: event.type
                    which: event.which
                    x: event.x
                    y: event.y
                    # currentTarget: event.currentTarget
                    # fromElement: event.fromElement
                    # path: event.path
                    # relatedTarget: event.relatedTarget
                    # shapeDef: event.shapeDef
                    # srcElement: event.srcElement
                    # stopImmediatePropagation: event.stopImmediatePropagation
                    # stopPropagation: event.stopPropagation
                    # symbol: event.symbol
                    # target: event.target
                    # toElement: event.toElement
                    # view: event.view

                base =
                    tag:      evt.tag.substring(0, evt.tag.length - 5)
                    contents: evt

                target ?= []

                @nodeEditor.pushViewEvent
                    path:   path
                    target: target
                    base:   base
                    
        # try
        #     nodeEditorBaseGL.install mountPoint2
        # catch e
        #     console.error e
    @content: ->
        uniqueTabNo++
        mountPoint = "luna-studio-mount" + uniqueTabNo
        @div =>
            @div
                style: 'display: flex; width: 100%; height: 100%'
                =>
                    @div
                        id: mountPoint2
                        style: 'flex-direction:row; width: 50%; height: inherit'
                        class: 'luna-studio-mount luna-studio luna-noselect'
                        tabindex: -1
                        =>
                            @h1 "Loading ..."
                    @div
                        id: mountPoint
                        style: 'flex-direction:row; width: 50%; height: inherit'
                        class: 'luna-studio-mount'
                        =>
                            @h1 "Loading ..."

    # getTitle:     -> path.basename(@uri)
    getTitle:     -> 'Node editor'

    handleEvents: =>
        atom.commands.add @element,
            'core:cancel':              -> pushShortcutEvent("Cancel")
            'core:accept':              -> pushShortcutEvent("Accept")
            'core:close': (e)           => @handleClose(e)
            'core:save': (e)            => @handleSave(e)
            # camera
            'luna-studio:center-graph': -> pushShortcutEvent("CenterGraph")
            'luna-studio:pan-down':     -> pushShortcutEvent("PanDown")
            'luna-studio:pan-left':     -> pushShortcutEvent("PanLeft")
            'luna-studio:pan-right':    -> pushShortcutEvent("PanRight")
            'luna-studio:pan-up':       -> pushShortcutEvent("PanUp")
            'luna-studio:reset-camera': -> pushShortcutEvent("ResetCamera")
            'luna-studio:reset-pan':    -> pushShortcutEvent("ResetPan")
            'luna-studio:reset-zoom':   -> pushShortcutEvent("ResetZoom")
            'luna-studio:zoom-in':      -> pushShortcutEvent("ZoomIn")
            'luna-studio:zoom-out':     -> pushShortcutEvent("ZoomOut")
            # clipboard
            'luna-studio:copy':  -> pushShortcutEvent("Copy")
            'luna-studio:cut':   -> pushShortcutEvent("Cut")
            'luna-studio:paste': -> pushShortcutEvent("Paste", atom.clipboard.read())
            # navigation
            'luna-studio:exit-graph':    -> pushShortcutEvent("ExitGraph")
            'luna-studio:go-cone-down':  -> pushShortcutEvent("GoConeDown")
            'luna-studio:go-cone-left':  -> pushShortcutEvent("GoConeLeft")
            'luna-studio:go-cone-right': -> pushShortcutEvent("GoConeRight")
            'luna-studio:go-cone-up':    -> pushShortcutEvent("GoConeUp")
            'luna-studio:go-down':       -> pushShortcutEvent("GoDown")
            'luna-studio:go-left':       -> pushShortcutEvent("GoLeft")
            'luna-studio:go-next':       -> pushShortcutEvent("GoNext")
            'luna-studio:go-prev':       -> pushShortcutEvent("GoPrev")
            'luna-studio:go-right':      -> pushShortcutEvent("GoRight")
            'luna-studio:go-up':         -> pushShortcutEvent("GoUp")
            # nodes
            'luna-studio:autolayout-all-nodes':        -> pushShortcutEvent("AutolayoutAllNodes")
            'luna-studio:autolayout-selected-nodes':   -> pushShortcutEvent("AutolayoutSelectedNodes")
            'luna-studio:close-visualization-preview': -> pushShortcutEvent("CloseVisualizationPreview")
            'luna-studio:collapse-to-function':        -> pushShortcutEvent("CollapseToFunction")
            'luna-studio:edit-selected-nodes':         -> pushShortcutEvent("EditSelectedNodes")
            'luna-studio:expand-selected-nodes':       -> pushShortcutEvent("ExpandSelectedNodes")
            'luna-studio:open-visualization-preview':  -> pushShortcutEvent("OpenVisualizationPreview")
            'luna-studio:remove-selected-nodes':       -> pushShortcutEvent("RemoveSelectedNodes")
            'luna-studio:select-all':                  -> pushShortcutEvent("SelectAll")
            'luna-studio:unfold-selected-nodes':       -> pushShortcutEvent("UnfoldSelectedNodes")
            'luna-studio:zoom-visualization':          -> pushShortcutEvent("ZoomVisualization")
            # undo/redo
            'core:redo': -> pushShortcutEvent("Redo")
            'core:undo': -> pushShortcutEvent("Undo")
            # MockMonads
            'luna-studio:mock-add-monad':    -> pushShortcutEvent("MockAddMonad")
            'luna-studio:mock-clear-monads': -> pushShortcutEvent("MockClearMonads")
            # searcher
            'luna-studio:searcher-open': (e)        -> pushShortcutEvent("SearcherOpen", e.detail)
            'luna-studio:searcher-edit-expression': -> pushShortcutEvent("SearcherEditExpression")
            # debug
            'luna-studio:debug-layer-0': -> pushShortcutEvent("EnableDebugLayer", "0")
            'luna-studio:debug-layer-1': -> pushShortcutEvent("EnableDebugLayer", "1")
            'luna-studio:debug-layer-2': -> pushShortcutEvent("EnableDebugLayer", "3")
            'luna-studio:debug-layer-3': -> pushShortcutEvent("EnableDebugLayer", "2")
            'luna-studio:debug-layer-4': -> pushShortcutEvent("EnableDebugLayer", "4")
            'luna-studio:debug-layer-5': -> pushShortcutEvent("EnableDebugLayer", "5")
            'luna-studio:debug-layer-6': -> pushShortcutEvent("EnableDebugLayer", "6")
            'luna-studio:debug-layer-7': -> pushShortcutEvent("EnableDebugLayer", "7")
            'luna-studio:debug-layer-8': -> pushShortcutEvent("EnableDebugLayer", "8")
            'luna-studio:debug-layer-9': -> pushShortcutEvent("EnableDebugLayer", "9")

        atom.commands.add '.luna-searcher',
            # searcher
            'luna-studio:searcher-accept-0':     -> pushSearcherEvent("HintShortcut", 0)
            'luna-studio:searcher-accept-1':     -> pushSearcherEvent("HintShortcut", 1)
            'luna-studio:searcher-accept-2':     -> pushSearcherEvent("HintShortcut", 2)
            'luna-studio:searcher-accept-3':     -> pushSearcherEvent("HintShortcut", 3)
            'luna-studio:searcher-accept-4':     -> pushSearcherEvent("HintShortcut", 4)
            'luna-studio:searcher-accept-5':     -> pushSearcherEvent("HintShortcut", 5)
            'luna-studio:searcher-accept-6':     -> pushSearcherEvent("HintShortcut", 6)
            'luna-studio:searcher-accept-7':     -> pushSearcherEvent("HintShortcut", 7)
            'luna-studio:searcher-accept-8':     -> pushSearcherEvent("HintShortcut", 8)
            'luna-studio:searcher-accept-9':     -> pushSearcherEvent("HintShortcut", 9)
            'luna-studio:searcher-accept-input': -> pushSearcherEvent("AcceptInput")
            'luna-studio:searcher-accept':       -> pushSearcherEvent("Accept")
            'luna-studio:searcher-tab-pressed':  -> pushSearcherEvent("TabPressed")
            'luna-studio:searcher-move-down':    -> pushSearcherEvent("MoveDown")
            'luna-studio:searcher-move-left':    -> pushSearcherEvent("MoveLeft")
            'luna-studio:searcher-move-up':      -> pushSearcherEvent("MoveUp")


    handleClose: (e) =>
        e.preventDefault()
        e.stopImmediatePropagation()

    handleSave: (e) =>
        e.preventDefault()
        e.stopImmediatePropagation()
        if @uri?
            @codeEditor.pushInternalEvent(tag: "SaveFile", _path: @uri)
            oldPath = atom.project.getPaths()[0]
            projects.temporaryProject.save (newPath) =>
                @codeEditor.pushInternalEvent(tag: 'MoveProject', _oldPath : oldPath, _newPath: newPath)
