
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
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:accept':        -> code.pushEvent("Accept")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:auto-zoom':     -> code.pushEvent("AutoZoom")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:cancel':        -> code.pushEvent("Cancel")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:delete':        -> code.pushEvent("Delete")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:open-searcher': -> code.pushEvent("OpenSearcher")
    @subs.add atom.commands.add '.luna-studio', 'luna-studio:select-all':    -> code.pushEvent("SelectAll")

  deactivate: ->
    @subs.dispose()
