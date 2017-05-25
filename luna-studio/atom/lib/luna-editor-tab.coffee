{TextEditor, TextBuffer} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
path = require 'path'
SubAtom = require 'sub-atom'
LunaSemanticGrammar = require './luna-grammar'

TextBuffer::onDidStopChangingWithDiffs = (callback) ->
  diffs = []
  disposed = no
  sub = @onDidChange (diff) ->
    diff_ext =
        uri: buffer.file.path
        start: buffer.characterIndexForPosition(diff.oldRange.start)
        end: buffer.characterIndexForPosition(diff.oldRange.start) + diff.oldText.length
        text: diff.newText
        cursor: buffer.characterIndexForPosition(editor.getCursorBufferPosition())
    diffs.push diff_ext
    clearTimeout changeTimeout if changeTimeout
    changeTimeout = setTimeout ->
      changeTimeout = null
      callback diffs if not disposed
      diffs = []
    , 300
  dispose: ->
    disposed = yes
    sub.dispose()


subscribe = null

module.exports =
  class LunaEditorTab extends TextEditor

    constructor: (@uri, @internal) ->

        super
        @getBuffer().setPath(@uri)

        @internal.pushInternalEvent(event: "GetBuffer", uri: @uri)

        withoutTrigger = (callback) =>
            @triggerPush = false
            callback()
            @triggerPush = true
        setBuffer = (uri_send, text) =>
            console.log(uri_send, @uri)
            withoutTrigger =>
              if @uri == uri_send
                @getBuffer().setText(text)
                console.log("setBuffer")
                @setGrammar(new LunaSemanticGrammar(atom.grammars, @internal.lex))

        @internal.bufferListener setBuffer

        setCode = (uri_send, start_send, end_send, text) =>
            withoutTrigger =>
              if @uri == uri_send
              #   start = @getBuffer().positionForCharacterIndex(start_send)
              #   end = @getBuffer().positionForCharacterIndex(end_send)
              #   @getBuffer().setTextInRange [start, end], text
              #   @.scrollToBufferPosition(start)
                  @getBuffer().setText(text)
        @internal.codeListener setCode

        @subscribe = new SubAtom
        @subscribe.add @getBuffer().onDidChange (event) =>
            return unless @triggerPush
            if event.newText != '' or event.oldText != ''
                diff =
                    uri: @uri
                    start: @getBuffer().characterIndexForPosition(event.oldRange.start)
                    end: @getBuffer().characterIndexForPosition(event.oldRange.start) + event.oldText.length
                    text: event.newText
                    cursor: @getBuffer().characterIndexForPosition(@.getCursorBufferPosition())
                  #   cursor: (@getBuffer().characterIndexForPosition(x) for x in @.getCursorBufferPositions()) #for multiple cursors
                @internal.pushText(diff)

    serialize: -> { deserializer: 'LunaEditorTab', uri: @uri }

    getTitle: -> path.basename(@uri)

    deactivate: ->
      @subscribe.dispose()