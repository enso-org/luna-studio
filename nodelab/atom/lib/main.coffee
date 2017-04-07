LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'

i             = require "./gen/ghcjs-code2.js"
internal = i()
c             = require "./gen/ghcjs-code.js"
code = c()
path = require 'path'

{TextEditor} = require 'atom'

TextEditor::onDidStopChangingWithDiffs = (callback) ->
  diffs = []
  disposed = no
  sub = @onDidChange (diff) ->
    diffs.push diff
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
  activate: ->
    internal.start()

    actStatus = (data) ->
        if data == 'activate'
            rootPath = atom.project.getPaths().shift()
            if rootPath != ""
                internal.pushInternalEvent("SetProject " + rootPath)
    internal.statusListener actStatus


    @observed = new SubAtom
    @observed.add atom.workspace.observeTextEditors (@editor) ->
        if @editor.buffer && editor.buffer.file
            uri = @editor.buffer.file.path
            if path.extname(uri) is '.luna'
                console.log(uri)
                internal.pushInternalEvent("OpenFile " + uri)

                @buffer = @editor.buffer

                console.log("in observed")
                console.log(editor)
                @triggerPush = true
                withoutTrigger = (callback) ->
                    @triggerPush = false
                    callback()
                    @triggerPush = true
                setCode = (uri_send, start_send, end_send, text) ->
                  withoutTrigger =>
                    if uri == uri_send
                      start = buffer.positionForCharacterIndex(start_send)
                      end = buffer.positionForCharacterIndex(end_send)
                      @buffer.setTextInRange [start, end], text
                      @editor.scrollToBufferPosition(start)
                internal.codeListener setCode
                @sss = new SubAtom
                @sss.add @buffer.onDidChange (event) =>
                    console.log(event)
                    return unless @triggerPush
                    if event.newText != '' or event.oldText != ''
                        diff =
                            uri: uri
                            start: buffer.characterIndexForPosition(event.oldRange.start)
                            end: buffer.characterIndexForPosition(event.oldRange.start) + event.oldText.length
                            text: event.newText
                            cursor: buffer.characterIndexForPosition(@editor.getCursorBufferPosition())
                        console.log(diff, event.oldRange.start, event.oldRange.end)
                        internal.pushText(diff)
                @sss.add @buffer.onWillSave (event) => internal.pushInternalEvent("SaveFile " + uri)
                @sss.add atom.workspace.onDidDestroyPaneItem (event) => #console.log(event.item.buffer.file.path)
                  if event.item.buffer
                      activeFilePath = event.item.buffer.file.path
                  else activeFilePath = event.item.uri
                  if path.extname(activeFilePath) is '.luna'
                      internal.pushInternalEvent("CloseFile " + activeFilePath)





    atom.workspace.addOpener (uri) ->

      if path.extname(uri) is '.luna'
        internal.pushInternalEvent("OpenFile " + uri)

        atom.workspace.open().then (@editor) ->
          @buffer = @editor.buffer
          @buffer.setPath(uri)
          internal.pushInternalEvent("GetBuffer " + uri)
          withoutTrigger = (callback) ->
            @triggerPush = false
            callback()
            @triggerPush = true

          setBuffer = (uri_send, text) ->
            withoutTrigger =>
              if uri == uri_send
                @buffer.setText(text)
          internal.bufferListener setBuffer

          setCode = (uri_send, start_send, end_send, text) ->
            withoutTrigger =>
              if uri == uri_send
                start = buffer.positionForCharacterIndex(start_send)
                end = buffer.positionForCharacterIndex(end_send)
                @buffer.setTextInRange [start, end], text
                @editor.scrollToBufferPosition(start)
          internal.codeListener setCode

          isSaved = (type, uri_send, status) ->
              if uri == uri_send and type == "IsSaved" and status =="True"
                  internal.pushInternalEvent("CloseFile " + activeFilePath)
                  atom.workspaceView.getActivePaneItem().destroy()
            #   else if uri == uri_send and type == "IsSaved" and status =="False"
                # internal.pushInternalEvent("SaveFile " + uri)
                # na plik zapisal sie zrob destroy i wyslij close file
                  # prompt window to ask if the file should be saved, send save


          @ss = new SubAtom
          @ss.add @buffer.onDidChange (event) =>
              console.log(event)
              return unless @triggerPush
              if event.newText != '' or event.oldText != ''
                  diff =
                      uri: uri
                      start: buffer.characterIndexForPosition(event.oldRange.start)
                      end: buffer.characterIndexForPosition(event.oldRange.start) + event.oldText.length
                      text: event.newText
                      cursor: buffer.characterIndexForPosition(@editor.getCursorBufferPosition())
                  console.log(diff, event.oldRange.start, event.oldRange.end)
                  internal.pushText(diff)
          @ss.add @buffer.onWillSave (event) => internal.pushInternalEvent("SaveFile " + uri)
          @ss.add @buffer.onWillReload (event) => internal.pushInternalEvent("GetBuffer " + uri)
          @ss.add atom.workspace.onDidDestroyPaneItem (event) => #console.log(event.item.buffer.file.path)
            if event.item.buffer
                activeFilePath = event.item.buffer.file.path
            else activeFilePath = event.item.uri
            if path.extname(activeFilePath) is '.luna'
                internal.pushInternalEvent("CloseFile " + activeFilePath)
                # internal.statusListener isSaved

        atom.workspace.getActivePane().activateItem new LunaStudioTab(uri, code)










    @subs = new SubAtom
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel': -> code.pushEvent("Shortcut Cancel")
    @subs.add atom.commands.add 'atom-workspace', 'core:close': ->
        if atom.workspace.getActivePaneItem().buffer
            activeFilePath =  atom.workspace.getActivePaneItem().buffer.file.path
        else activeFilePath = atom.workspace.getActivePane().activeItem.uri
        if path.extname(activeFilePath) is ".luna"
            internal.pushInternalEvent("CloseFile " + activeFilePath)
    @subs.add atom.commands.add 'atom-workspace', 'core:save', (e)                 ->
      if atom.workspace.getActivePaneItem().buffer
          activeFilePath =  atom.workspace.getActivePaneItem().buffer.file.path
      else activeFilePath = atom.workspace.getActivePane().activeItem.uri
      if path.extname(activeFilePath) is ".luna"
        e.preventDefault()
        e.stopImmediatePropagation()
        internal.pushInternalEvent("SaveFile " + activeFilePath)
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel':       -> code.pushEvent("Shortcut Cancel")
    # camera
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:center-graph': -> code.pushEvent("Shortcut CenterGraph")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-down':     -> code.pushEvent("Shortcut PanDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-left':     -> code.pushEvent("Shortcut PanLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-right':    -> code.pushEvent("Shortcut PanRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-up':       -> code.pushEvent("Shortcut PanUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-camera': -> code.pushEvent("Shortcut ResetCamera")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-pan':    -> code.pushEvent("Shortcut ResetPan")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-zoom':   -> code.pushEvent("Shortcut ResetZoom")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-in':      -> code.pushEvent("Shortcut ZoomIn")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-out':     -> code.pushEvent("Shortcut ZoomOut")
    # clipboard
    @subs.add atom.commands.add '.luna-studio', 'core:copy':  -> code.pushEvent("Shortcut Copy")
    @subs.add atom.commands.add '.luna-studio', 'core:cut':   -> code.pushEvent("Shortcut Cut")
    @subs.add atom.commands.add '.luna-studio', 'core:paste': -> code.pushEvent("Shortcut Paste \"" + atom.clipboard.read().replace(/["]+/g, '\\\"') +  "\"")
    # navigation
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:exit-graph':    -> code.pushEvent("Shortcut ExitGraph")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-down':  -> code.pushEvent("Shortcut GoConeDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-left':  -> code.pushEvent("Shortcut GoConeLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-right': -> code.pushEvent("Shortcut GoConeRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-up':    -> code.pushEvent("Shortcut GoConeUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-down':       -> code.pushEvent("Shortcut GoDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-left':       -> code.pushEvent("Shortcut GoLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-next':       -> code.pushEvent("Shortcut GoNext")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-prev':       -> code.pushEvent("Shortcut GoPrev")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-right':      -> code.pushEvent("Shortcut GoRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-up':         -> code.pushEvent("Shortcut GoUp")
    # nodes
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:edit-selected-nodes':   -> code.pushEvent("Shortcut EditSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:expand-selected-nodes': -> code.pushEvent("Shortcut ExpandSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:remove-selected-nodes': -> code.pushEvent("Shortcut RemoveSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:select-all':            -> code.pushEvent("Shortcut SelectAll")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:unfold-selected-nodes': -> code.pushEvent("Shortcut UnfoldSelectedNodes")
    # searcher
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:searcher-open':           -> code.pushEvent("Shortcut SearcherOpen")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-0':     -> code.pushEvent("Searcher AcceptEntry 0")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-1':     -> code.pushEvent("Searcher AcceptEntry 1")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-2':     -> code.pushEvent("Searcher AcceptEntry 2")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-3':     -> code.pushEvent("Searcher AcceptEntry 3")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-4':     -> code.pushEvent("Searcher AcceptEntry 4")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-5':     -> code.pushEvent("Searcher AcceptEntry 5")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-6':     -> code.pushEvent("Searcher AcceptEntry 6")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-7':     -> code.pushEvent("Searcher AcceptEntry 7")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-8':     -> code.pushEvent("Searcher AcceptEntry 8")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-9':     -> code.pushEvent("Searcher AcceptEntry 9")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept-input': -> code.pushEvent("Searcher AcceptInput")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept':       -> code.pushEvent("Searcher Accept")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-down':    -> code.pushEvent("Searcher MoveDown")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-left':    -> code.pushEvent("Searcher MoveLeft")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-right':   -> code.pushEvent("Searcher MoveRight")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-up':      -> code.pushEvent("Searcher MoveUp")
    # undo/redo
    @subs.add atom.commands.add '.luna-studio', 'core:redo': -> code.pushEvent("Shortcut Redo")
    @subs.add atom.commands.add '.luna-studio', 'core:undo': -> code.pushEvent("Shortcut Undo")

  deactivate: ->
    @subs.dispose()
    @ss.dispose()
    @sss.dispose()
    @observed.dispose()
