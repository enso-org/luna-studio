
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'
c             = require "./gen/ghcjs-code.js"
code = c()

module.exports =
  activate: ->
    @subs = new SubAtom
    @subs.add atom.commands.add 'atom-workspace', 'luna-studio:open': ->
      atom.workspace.getActivePane().activateItem new LunaStudioTab "Luna Studio"
      code.start()
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-left':              -> code.pushEvent("PanLeft")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-right':             -> code.pushEvent("PanRight")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-up':                -> code.pushEvent("PanUp")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:pan-down':              -> code.pushEvent("PanDown")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-in':               -> code.pushEvent("ZoomIn")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-in':               -> code.pushEvent("ZoomIn")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-out':              -> code.pushEvent("ZoomOut")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:zoom-out':              -> code.pushEvent("ZoomOut")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-zoom':            -> code.pushEvent("ResetZoom")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-pan':             -> code.pushEvent("ResetPan")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:reset-camera':          -> code.pushEvent("ResetCamera")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:center-graph':          -> code.pushEvent("CenterGraph")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:center-graph':          -> code.pushEvent("CenterGraph")
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
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:select-all':            -> code.pushEvent("SelectAll")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:remove-selected-nodes': -> code.pushEvent("RemoveSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:unselect-sll':          -> code.pushEvent("UnselectAll")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:expand-selected-nodes': -> code.pushEvent("ExpandSelectedNodes")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:searcher-open':         -> code.pushEvent("SearcherOpen")
    @subs.add atom.commands.add '.searcher',    'luna-studio:searcher-accept':       -> code.pushEvent("SearcherAccept")
    @subs.add atom.commands.add '.searcher',    'luna-studio:searcher-close':        -> code.pushEvent("SearcherClose")
    @subs.add atom.commands.add '.searcher',    'luna-studio:searcher-close':        -> code.pushEvent("SearcherClose")
    @subs.add atom.commands.add '.searcher',    'luna-studio:searcher-move-down':    -> code.pushEvent("SearcherMoveDown")
    @subs.add atom.commands.add '.searcher',    'luna-studio:searcher-move-up':      -> code.pushEvent("SearcherMoveUp")

  deactivate: ->
    @subs.dispose()
