
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
                setCode = (diff) ->
                     @buffer.setText (diff)
                code.codeListener setCode
                listenToBufferChanges: ->
                  @buffer.onDidChange(event) =>
                    if !(event.newText is "\n") and (event.newText.length is 0)
                      changeType = 'deletion'
                      event = {oldRange: event.oldRange}
                    else if event.oldRange.containsRange(event.newRange) or event.newRange.containsRange(event.oldRange)
                      changeType = 'substitution'
                      event = {oldRange: event.oldRange, newRange: event.newRange, newText: event.newText}
                    else
                      changeType = 'insertion'
                      event = {newRange: event.newRange, newText: event.newText}
            atom.workspace.getActivePane().activateItem new LunaStudioTab(uri, code)

    @subs = new SubAtom
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel':                -> code.pushEvent("Shortcut Cancel")
    # camera
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-left':              -> code.pushEvent("Shortcut PanLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-right':             -> code.pushEvent("Shortcut PanRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-up':                -> code.pushEvent("Shortcut PanUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-down':              -> code.pushEvent("Shortcut PanDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-in':               -> code.pushEvent("Shortcut ZoomIn")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-out':              -> code.pushEvent("Shortcut ZoomOut")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-zoom':            -> code.pushEvent("Shortcut ResetZoom")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-pan':             -> code.pushEvent("Shortcut ResetPan")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-camera':          -> code.pushEvent("Shortcut ResetCamera")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:center-graph':          -> code.pushEvent("Shortcut CenterGraph")
    # clipboard
    @subs.add atom.commands.add '.luna-studio', 'core:copy':                         -> code.pushEvent("Shortcut Copy")
    @subs.add atom.commands.add '.luna-studio', 'core:cut':                          -> code.pushEvent("Shortcut Cut")
    @subs.add atom.commands.add '.luna-studio', 'core:paste':                        -> code.pushEvent("Shortcut Paste \"" + atom.clipboard.read().replace(/["]+/g, '\\\"') +  "\"")
    # navigation
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-prev':               -> code.pushEvent("Shortcut GoPrev")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-next':               -> code.pushEvent("Shortcut GoNext")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-left':               -> code.pushEvent("Shortcut GoLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-up':                 -> code.pushEvent("Shortcut GoUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-right':              -> code.pushEvent("Shortcut GoRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-down':               -> code.pushEvent("Shortcut GoDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-left':          -> code.pushEvent("Shortcut GoConeLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-up':            -> code.pushEvent("Shortcut GoConeUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-right':         -> code.pushEvent("Shortcut GoConeRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-down':          -> code.pushEvent("Shortcut GoConeDown")
    # nodes
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:select-all':            -> code.pushEvent("Shortcut SelectAll")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:remove-selected-nodes': -> code.pushEvent("Shortcut RemoveSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:expand-selected-nodes': -> code.pushEvent("Shortcut ExpandSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:edit-selected-nodes':   -> code.pushEvent("Shortcut EditSelectedNodes")
    # searcher
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:searcher-open':         -> code.pushEvent("Shortcut SearcherOpen")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept':     -> code.pushEvent("Searcher Accept")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-right': -> code.pushEvent("Searcher MoveRight")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-left':  -> code.pushEvent("Searcher MoveLeft")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-down':  -> code.pushEvent("Searcher MoveDown")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-up':    -> code.pushEvent("Searcher MoveUp")
    # undo/redo
    @subs.add atom.commands.add '.luna-studio', 'core:undo':                         -> code.pushEvent("Shortcut Undo")
    @subs.add atom.commands.add '.luna-studio', 'core:redo':                         -> code.pushEvent("Shortcut Redo")

  deactivate: ->
    @subs.dispose()
