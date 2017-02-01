
{View} = require 'atom-space-pen-views'

module.exports =
class LunaStudioTabView extends View

  @content: ->
    @div
      id: 'luna-studio-mount'
      =>
        @h1 "Loading ..."
