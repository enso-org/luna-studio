
{View} = require 'atom-space-pen-views'

module.exports =
class LunaStudioTabView extends View

  @content: ->
    @div
      id: 'mount-point'
      =>
        @h1 "Loading ..."
