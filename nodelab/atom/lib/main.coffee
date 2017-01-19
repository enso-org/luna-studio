
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'

module.exports =
  activate: ->
    @subs = new SubAtom
    @subs.add atom.commands.add 'atom-workspace', 'luna-studio:open': ->
      atom.workspace.getActivePane().activateItem new LunaStudioTab "Luna Studio!!!"
      # require "./ghcjs-code.js"


  deactivate: ->
    @subs.dispose()
