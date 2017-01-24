
LunaStudioTabView = require './luna-studio-tab-view'

module.exports =
class LunaStudioTab
  constructor: (@tabTitle) ->

  getTitle:     -> @tabTitle
  getViewClass: -> LunaStudioTabView
