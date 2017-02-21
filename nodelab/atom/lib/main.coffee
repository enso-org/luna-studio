
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
            handleNotification = (error) ->
              atom.notifications.addError(error)
            code.notificationListener handleNotification


            atom.workspace.getActivePane().activateItem new LunaStudioTab(uri, code)


    @subs = new SubAtom
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel':                -> code.pushEvent("Cancel")
    # camera
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-left':              -> code.pushEvent("PanLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-right':             -> code.pushEvent("PanRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-up':                -> code.pushEvent("PanUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-down':              -> code.pushEvent("PanDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-in':               -> code.pushEvent("ZoomIn")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-out':              -> code.pushEvent("ZoomOut")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-zoom':            -> code.pushEvent("ResetZoom")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-pan':             -> code.pushEvent("ResetPan")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-camera':          -> code.pushEvent("ResetCamera")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:center-graph':          -> code.pushEvent("CenterGraph")
    # clipboard
    @subs.add atom.commands.add '.luna-studio', 'core:copy':                         -> code.pushEvent("Copy")
    @subs.add atom.commands.add '.luna-studio', 'core:cut':                          -> code.pushEvent("Cut")
    @subs.add atom.commands.add '.luna-studio', 'core:paste':                        -> code.pushEvent("Paste \"" + atom.clipboard.read().replace(/["]+/g, '\\\"') +  "\"")
    # navigation
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-prev':               -> code.pushEvent("GoPrev")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-next':               -> code.pushEvent("GoNext")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-left':               -> code.pushEvent("GoLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-up':                 -> code.pushEvent("GoUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-right':              -> code.pushEvent("GoRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-down':               -> code.pushEvent("GoDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-left':          -> code.pushEvent("GoConeLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-up':            -> code.pushEvent("GoConeUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-right':         -> code.pushEvent("GoConeRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:go-cone-down':          -> code.pushEvent("GoConeDown")
    # nodes
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:select-all':            -> code.pushEvent("SelectAll")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:remove-selected-nodes': -> code.pushEvent("RemoveSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:expand-selected-nodes': -> code.pushEvent("ExpandSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:edit-selected-nodes':   -> code.pushEvent("EditSelectedNodes")

    # searcher
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:searcher-open':         -> code.pushEvent("SearcherOpen")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-accept':     -> code.pushEvent("SearcherAccept")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-down':  -> code.pushEvent("SearcherMoveDown")
    @subs.add atom.commands.add '.luna-searcher', 'luna-studio:searcher-move-up':    -> code.pushEvent("SearcherMoveUp")
    # undo/redo
    @subs.add atom.commands.add '.luna-studio', 'core:undo':                         -> code.pushEvent("Undo")
    @subs.add atom.commands.add '.luna-studio', 'core:redo':                         -> code.pushEvent("Redo")

  deactivate: ->
    @subs.dispose()
