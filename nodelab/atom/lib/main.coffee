# w         = require './gen/websocket'
# websocket = w()
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'

i             = require "./gen/ghcjs-code2.js"
internal = i()
c             = require "./gen/ghcjs-code.js"
code = c()
path = require 'path'


module.exports =
  activate: ->
    internal.start()
    rootPath = atom.project.getPaths().shift()
    # if rootPath != ""
    #   internal.pushInternalEvent("SetProject " + rootPath)
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

          @ss = new SubAtom
          @ss.add @buffer.onDidChange (event) =>
              return unless @triggerPush
              if event.newText != '' or event.oldText != ''
                  diff =
                      uri: uri
                      start: buffer.characterIndexForPosition(event.oldRange.start)
                      end: buffer.characterIndexForPosition(event.oldRange.end)
                      text: event.newText
                      cursor: buffer.characterIndexForPosition(@editor.getCursorBufferPosition())
              internal.pushText(diff)
          @ss.add @buffer.onWillSave (event) => internal.pushInternalEvent("SaveFile " + uri)

        atom.workspace.getActivePane().activateItem new LunaStudioTab(uri, code)


    @subs = new SubAtom
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel': -> code.pushEvent("Shortcut Cancel")
    @subs.add atom.commands.add 'atom-workspace', 'core:close': ->
        if atom.workspace.getActivePaneItem().buffer
            activeFilePath =  atom.workspace.getActivePaneItem().buffer.file.path
        else activeFilePath = atom.workspace.getActivePane().activeItem.uri
        # console.log(activeFilePath)
        if path.extname(activeFilePath) is ".luna"
            internal.pushInternalEvent("CloseFile " + activeFilePath)
    @subs.add atom.commands.add 'atom-workspace', 'core:save', (e)                 ->
      if atom.workspace.getActivePaneItem().buffer
          activeFilePath =  atom.workspace.getActivePaneItem().buffer.file.path
      else activeFilePath = atom.workspace.getActivePane().activeItem.uri
      if path.extname(activeFilePath) is ".luna"
        # console.log("core save" + activeFilePath)
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
