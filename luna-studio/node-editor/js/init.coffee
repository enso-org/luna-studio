$$              = require('./common')
websocket       = require('./websocket')
$$.websocket = websocket()

module.exports =
  websocket: $$.websocket

window.processedEvents = []
