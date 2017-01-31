DragDrop = require('./DragDrop')

module.exports =
  initializeEvents: ->
    DragDrop.initDragDrop()
    setInterval (-> module.exports.customEvent "tick", null), 1000
  customEvent: -> null
