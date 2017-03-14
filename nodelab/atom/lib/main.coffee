
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'
c             = require "./gen/ghcjs-code.js"
code = c()
path = require 'path'


module.exports =
  activate: ->
    atom.workspace.addOpener (uri) ->
        if path.extname(uri) is '.luna'
            atom.workspace.open().then (@editor) ->
                @buffer = @editor.buffer
                listenToBufferChanges = ->
                  @buffer.onDidChange(event) =>
                    if !(event.newText is "\n") and (event.newText.length is 0)
                      changeType : 'deletion'
                      event : {oldRange: event.oldRange}
                      cursor : @buffer.getCursorBufferPosition()
                    else if event.oldRange.containsRange(event.newRange) or event.newRange.containsRange(event.oldRange)
                      changeType : 'substitution'
                      event : {oldRange: event.oldRange, newRange: event.newRange, newText: event.newText}
                      cursor : @buffer.getCursorBufferPosition()
                    else
                      changeType : 'insertion'
                      event : {newRange: event.newRange, newText: event.newText}
                      cursor : @buffer.getCursorBufferPosition()
                code.pushInternalEvent listenToBufferChanges
                changeBuffer = (data) ->
                  if data.event.newRange then newRange = Range.fromObject(data.event.newRange)
                  if data.event.oldRange then oldRange = Range.fromObject(data.event.oldRange)
                  if data.event.newText then newText = data.event.newText

                  switch data.changeType
                    when 'deletion'
                      @buffer.delete oldRange
                      actionArea = oldRange.start
                    when 'substitution'
                       @buffer.setTextInBufferRange oldRange, newText
                       actionArea = oldRange.start
                    else
                      @buffer.insert newRange.start, newText
                      actionArea = newRange.start
                code.codeListener changeBuffer

            atom.workspace.getActivePane().activateItem new LunaStudioTab(uri, code)


    @subs = new SubAtom
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel': -> code.pushEvent("Shortcut Cancel")
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
