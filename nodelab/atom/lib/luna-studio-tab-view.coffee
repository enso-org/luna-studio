
{View} = require 'atom-space-pen-views'

module.exports =
class LunaStudioTabView extends View

  @content: ->
    @div
      id: 'nodelab-app'
      =>
        @h1 "Loading ..."
