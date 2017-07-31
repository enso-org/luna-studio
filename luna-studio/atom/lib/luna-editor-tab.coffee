{TextEditor, TextBuffer} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
{CompositeDisposable} = require 'event-kit'
path = require 'path'
SubAtom = require 'sub-atom'

TextBuffer::subscribeToFileOverride = (internal) ->
    @fileSubscriptions?.dispose()
    @fileSubscriptions = new CompositeDisposable

    @fileSubscriptions.add @file.onDidChange =>
        @conflict = true if @isModified()
        console.log "FileChanged"
        internal.pushInternalEvent(tag: "FileChanged", _path: @getPath())
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

subscribe = null

module.exports =
  class LunaEditorTab extends TextEditor

    constructor: (@uri, @internal) ->

        super
        @diffToOmit = new Set()
        @getBuffer().setPath(@uri)
        @getBuffer().subscribeToFileOverride(@internal)

        @internal.pushInternalEvent(tag: "GetBuffer", _path: @uri)

        omitDiff = (text) =>
            @diffToOmit.add(text)

        setBuffer = (uri_send, text) =>
            console.log(uri_send, @uri)
            if @uri == uri_send
                omitDiff(text)
                @getBuffer().setText(text)
                console.log("setBuffer")

        @internal.bufferListener setBuffer

        setCode = (uri_send, start_send, end_send, text) =>
            if @uri == uri_send
            #   start = @getBuffer().positionForCharacterIndex(start_send)
            #   end = @getBuffer().positionForCharacterIndex(end_send)
            #   @getBuffer().setTextInRange [start, end], text
            #   @.scrollToBufferPosition(start)
                omitDiff(text)
                @getBuffer().setText(text)
        @internal.codeListener setCode

        @subscribe = new SubAtom
        @subscribe.add @getBuffer().onDidStopChanging (event) =>
            for change in event.changes
                if @diffToOmit.has(change.newText)
                    @diffToOmit.delete(change.newText)
                else
                    diff =
                        # uri: @uri
                        start: change.start
                        end:   change.start.translate(change.oldExtent)
                        text:  change.newText
                        cursor: @.getCursorBufferPosition()
                      #   cursor: (@getBuffer().characterIndexForPosition(x) for x in @.getCursorBufferPositions()) #for multiple cursors
                    @internal.pushDiff(diff)

    serialize: -> { deserializer: 'LunaEditorTab', uri: @uri }

    getTitle: -> path.basename(@uri)

    deactivate: ->
        @subscribe.dispose()
