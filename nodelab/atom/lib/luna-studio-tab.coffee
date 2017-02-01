path = require 'path'
LunaStudioTabView = require './luna-studio-tab-view'


module.exports =
class LunaStudioTab
  constructor: (@uri, @code) ->
      @code.start()

  getTitle:     -> path.basename(@uri)
  getViewClass: -> LunaStudioTabView
