path = require 'path'
LunaStudioTabView = require './luna-studio-tab-view'


module.exports =
class LunaStudioTab
  constructor: (@uri, @code) ->
      @code.start(@uri, "mount-point")

  getTitle:     -> path.basename(@uri)
  getViewClass: -> LunaStudioTabView
