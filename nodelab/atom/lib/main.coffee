
LunaStudioTab = require './luna-studio-tab'
SubAtom       = require 'sub-atom'
# callback      = require './gen/atom-callback'
c          = require "./gen/ghcjs-code.js"
code = c()

module.exports =
  activate: ->
    @subs = new SubAtom
    @subs.add atom.commands.add 'atom-workspace', 'luna-studio:open': ->
      atom.workspace.getActivePane().activateItem new LunaStudioTab "Luna Studio"
      code.start()
    @subs.add atom.commands.add 'atom-workspace', 'luna-studio:test': ->
      code.pushEvent()

  deactivate: ->
    @subs.dispose()
