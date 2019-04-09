{TextEditor, TextBuffer, Range} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
{CompositeDisposable} = require 'event-kit'
path = require 'path'
SubAtom = require 'sub-atom'
Spinner = require './spinner'

TextBuffer::setModified = (@modified) ->

TextBuffer::isModified = ->
    if @modified?
        return @modified
    if @file # This is implementation for version 1.18
        return false unless @loaded

        if @file.existsSync()
            return @getText() isnt @cachedDiskContents
    not @isEmpty()

TextBuffer::isInConflict = ->
    if @modified?
        return false
    @isModified() and @fileHasChangedSinceLastLoad

TextBuffer::subscribeToFileOverride = (codeEditor) ->
    @fileSubscriptions?.dispose()
    @fileSubscriptions = new CompositeDisposable

    @fileSubscriptions.add @file.onDidChange =>
        @setModified(false)
        # @conflict = true if @isModified()
        codeEditor.pushInternalEvent(tag: "FileChanged", _path: @getPath())
      #   previousContents = @cachedDiskContents
      #
      #   # Synchrounously update the disk contents because the {File} has already cached them. If the
      #   # contents updated asynchrounously multiple `conlict` events could trigger for the same disk
      #   # contents.
      #   @updateCachedDiskContentsSync()
      #   return if previousContents == @cachedDiskContents
      #
      #   if @conflict
      #     @emitter.emit 'did-conflict'
      #   else
      #     @reload()

    @fileSubscriptions.add @file.onDidDelete =>
        modified = @getText() != @cachedDiskContents
        @wasModifiedBeforeRemove = modified
        @emitter.emit 'did-delete'
        if modified
            @updateCachedDiskContents()
        else
            @destroy()

    @fileSubscriptions.add @file.onDidRename =>
        @emitter.emit 'did-change-path', @getPath()

    @fileSubscriptions.add @file.onWillThrowWatchError (errorObject) =>
        @emitter.emit 'will-throw-watch-error', errorObject

################################################################################
################################## Hack Zone ###################################
################################################################################
## This section is a hack designed to get around Atom's event throttling.
## It is needed for programatically triggered changes, so that we know that
## those are indeed programatically triggered and can ignored them.
## It is mostly a blatant copy-paste from Atom repo, since the current (1.18.0)
## version does not expose this functionality.
## This code is unnecessary on newer versions of Atom and other editors.
## If the out-of-the-box buffer defines the `#emitDidStopChangingEvent` method,
## remove this code. Keep this in mind when maintaining this code.


normalizePatchChanges = (changes) ->
    changes.map (change) =>
        oldRange = new Range(change.oldStart, change.oldEnd)
        newRange = new Range(change.newStart, change.newEnd)
        oldText = change.oldText
        newText = change.newText
        new TextChange(oldRange, newRange, oldText, newText)

class TextChange
    constructor: (@oldRange, @newRange, @oldText, @newText) ->

Object.defineProperty TextChange.prototype, 'start',
    get: () ->
        @newRange.start
    enumerable: false

Object.defineProperty TextChange.prototype, 'oldExtent',
    get: () ->
        @oldRange.getExtent
    enumerable: false

Object.defineProperty TextChange.prototype, 'newExtent',
    get: () ->
        @newRange.getExtent
    enumerable: false

TextBuffer::emitDidStopChangingEvent = ->
    patches = @patchesSinceLastStoppedChangingEvent
    window.patches = patches
    @patchesSinceLastStoppedChangingEvent = []
    modifiedStatus = @isModified()
    for patch in patches
        @emitter.emit 'did-stop-changing',
            changes: Object.freeze(normalizePatchChanges(patch.getHunks()))
    @emitModifiedStatusChanged(modifiedStatus)

################################################################################
############################### End of Hack Zone ###############################
################################################################################


module.exports =
    class LunaCodeEditorTab extends TextEditor

        constructor: (@uri, @codeEditor, @projects) ->
            super
            @initialized = false
            @setModified false
            @setUri @uri
            @diffToOmit = new Set()
            @afterCodeChanged = []
            @setPlaceholderText 'Please wait'
            @codeEditor.pushInternalEvent(tag: 'OpenFile', _path: @uri)

            @codeEditor.onSetBuffer (uri, text) =>
                @setBuffer uri, text
                @initialize()
            @codeEditor.onSetClipboard @setClipboard
            @codeEditor.onInsertCode @insertCode
            @handleEvents()

            @subscribe = new SubAtom
            @subscribe.add @getBuffer().onDidStopChanging (e) => @handleDidStopChanging e
            spinner = new Spinner(progress = 0, overlap = true)
            @spinnerElement = @element.appendChild spinner.element

        initialize: ->
            unless @initialized
                @initialized = true
                @onInitialize?()

        serialize: -> { deserializer: 'LunaCodeEditorTab', uri: @uri }

        getTitle: -> path.basename(@uri)

        isLunaEditor: -> true

        setUri: (uri) =>
            @getBuffer().setPath(uri)
            @getBuffer().subscribeToFileOverride(@codeEditor)
            @uri = uri
        deactivate: -> @subscribe.dispose()

        handleEvents: =>
            atom.commands.add @element,
                'core:copy':  (e) => @handleCopy  e
                'core:cut':   (e) => @handleCut   e
                'core:paste': (e) => @handlePaste e
                'core:save':  (e) => @handleSave  e
                'core:undo':  (e) => @handleUndo e
                'core:redo':  (e) => @handleRedo e

        __spans: (markWholeLines) =>
            for s in @getSelections()
                head = s.marker.oldHeadBufferPosition
                tail = s.marker.oldTailBufferPosition
                if markWholeLines and head.isEqual tail
                    head.column = 0
                    tail.column = 0
                    tail.row += 1
                [head, tail]

        __convertSpans: (spans) =>
            buffer = @getBuffer()
            for s in spans
                head = s[0]
                tail = s[1]
                [buffer.characterIndexForPosition(head),
                 buffer.characterIndexForPosition(tail)].sort((a,b) -> a - b)

        spans: (markWholeLines) =>
            @__convertSpans @__spans markWholeLines

        handleDidStopChanging: (event) =>
                diffs = []
                for change in event.changes
                    if @diffToOmit.has change.newText
                        @diffToOmit.delete change.newText
                    else
                        @setModified(true)
                        start = change.oldRange.start
                        end   = change.oldRange.end
                        cursor = @getCursorBufferPosition()
                        diff =
                            _range:   [{_column: start.column, _row: start.row },
                                       {_column: end.column  , _row: end.row}]
                            _newText: change.newText
                            _cursor:  {_column: cursor.column, _row: cursor.row}
                          #   cursor: (@getBuffer().characterIndexForPosition(x) for x in @.getCursorBufferPositions()) #for multiple cursors
                        diffs.push diff
                if diffs.length > 0
                    @codeEditor.pushDiffs diffs
                for action in @afterCodeChanged
                    action()
                @afterCodeChanged = []

        forceStopChanging: (action) =>
            @afterCodeChanged.push action
            @getBuffer().scheduleDidStopChangingEvent()

        handleUndo: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                @codeEditor.pushInternalEvent(tag: "Undo")

        handleRedo: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                @codeEditor.pushInternalEvent(tag: "Redo")

        handleCopy: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                @codeEditor.pushInternalEvent(tag: "Copy", _path: @uri, _selections: @spans(true))

        handleCut: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                spans = @__spans true
                @codeEditor.pushInternalEvent(tag: "Copy", _path: @uri, _selections: @__convertSpans spans)
                spans.reverse().forEach (span) =>
                    @getBuffer().delete span

        handlePaste: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                cbd = atom.clipboard.readWithMetadata()
                cbdData = []
                if cbd.metadata? && cbd.metadata.selections?
                    for x in cbd.metadata.selections
                        cbdData.push(x.text)
                else
                    cbdData[0] = cbd.text
                @codeEditor.pushInternalEvent(tag: "Paste", _selections: @spans(), _content: cbdData)

        handleSave: (e) =>
            e.preventDefault()
            e.stopImmediatePropagation()
            @forceStopChanging =>
                @setModified(false)
                @codeEditor.pushInternalEvent(tag: "SaveFile", _path: @uri)
                oldPath = atom.project.getPaths()[0]
                @projects.temporaryProjectSave (newPath) =>
                    @codeEditor.pushInternalEvent(tag: 'MoveProject', _oldPath : oldPath, _newPath: newPath)

        insertCode: (uri, diffs) =>
            if @uri == uri
                selections = []
                for {_newText: text, _range: range, _cursor: cursor} in diffs
                    @omitDiff(text)
                    if range?
                        fixedRange = [[range[0]._row, range[0]._column],
                                      [range[1]._row, range[1]._column]]
                        @setTextInBufferRange(fixedRange, text)
                    else
                        @setText(text)
                    if cursor?
                        selections.push([[cursor._row, cursor._column],
                                         [cursor._row, cursor._column]])
                    @getBuffer().emitDidStopChangingEvent()
                if selections.length > 0
                    @setSelectedBufferRanges(selections)

        setClipboard: (uri, text) =>
            if @uri == uri
                atom.clipboard.write(text)

        setBuffer: (uri, text) =>
            if @uri == uri
                if @getPlaceholderText() != ''
                    @setPlaceholderText ''
                for child in @element.childNodes
                    if child.id == 'luna_logo-spinner'
                        @element.removeChild child
                        break
                unless @getBuffer().getText() is text
                    @omitDiff(text)
                    selections = @getSelectedBufferRanges()
                    @getBuffer().setText(text)
                    @setSelectedBufferRanges(selections)
                    console.log "setBuffer"

        setModified: (modified) =>
            @getBuffer().setModified(modified)
            @getBuffer().emitter.emit 'did-change-modified', modified

        omitDiff: (text) =>
            @diffToOmit.add(text)
