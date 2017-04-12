{TextEditor, TextBuffer} = require 'atom'
{TextEditorView, View} = require 'atom-space-pen-views'
path = require 'path'
SubAtom       = require 'sub-atom'

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



module.exports =
class LunaEditorTab extends TextEditor

  constructor: (@uri, @internal) ->

      super
      #@editor = atom.workspace.buildTextEditor()
    #   atom.workspace.open().then (@editor) ->
    #   @getBuffer = @getBuffer() #@editor.buffer
      @getBuffer().setPath(@uri)

      @internal.pushInternalEvent("GetBuffer " + @uri)

      withoutTrigger = (callback) =>
          @triggerPush = false
          callback()
          @triggerPush = true
      setBuffer = (uri_send, text) =>
          withoutTrigger =>
            console.log(@uri, @getBuffer())
            if @uri == uri_send
              @getBuffer().setText(text)
      @internal.bufferListener setBuffer

      setCode = (uri_send, start_send, end_send, text) ->
          withoutTrigger =>
            if uri == uri_send
              start = buffer.positionForCharacterIndex(start_send)
              end = buffer.positionForCharacterIndex(end_send)
              @getBuffer().setTextInRange [start, end], text
              @.scrollToBufferPosition(start)
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
              @internal.pushText(diff)
      @subscribe.add @buffer.onWillSave (event) => internal.pushInternalEvent("SaveFile " + uri)
      @subscribe.add @buffer.onWillReload (event) => internal.pushInternalEvent("GetBuffer " + uri)
      @subscribe.add atom.workspace.onDidDestroyPaneItem (event) => #console.log(event.item.buffer.file.path)
          if event.item.buffer
              activeFilePath = event.item.buffer.file.path
          else activeFilePath = event.item.uri
          if path.extname(activeFilePath) is '.luna'
              @internal.pushInternalEvent("CloseFile " + activeFilePath)
              # internal.statusListener isSaved


  getTitle: -> path.basename(@uri)
  deactivate: ->
    @subscribe.dispose()
